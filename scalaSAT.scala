object scalaSAT {

  type Literal = Int
  type Clause = Seq[Literal]
  type CNF = Seq[Clause]
  type Model = List[Literal]

  def parseFile(filename : String) : CNF = {
    val lines = scala.io.Source.fromFile(filename).getLines.toList
    val filterLines = lines.filter(x => x.head != 'p' && x.head != 'c')
    (for (l <- filterLines) yield {
      val lits = l.split(" ").map(_.toInt).filter(_ != 0)
      lits.toList
    }).toList
  }

  // CNF actions
  def containsEmptyClause(cnf : CNF) =
    cnf.exists(_.length == 0)

  def firstUnitClause(cnf : CNF) =
    cnf.find(_.length == 1)  

  def firstPureLiteral(cnf : CNF) = {
    val allSet = cnf.map(_.toSet).foldLeft(Set() : Set[Literal])(_ ++_)
    allSet.find(x => !(allSet contains x))
  }

  def firstLiteral(cnf : CNF) =
    cnf.head.head

  def assignLiteral(cnf : CNF, lit : Literal) =
    for (c <- cnf if (!(c contains lit))) yield
      for (l <- c; if (l != -lit)) yield l

  var backtracks = 0

  def DPLLaux(cnf : CNF, partialModel : Model) : Option[Model] = {
    // if Φ is a consistent set of literals
    if (cnf.isEmpty) {
      println("Empty")
      Some(partialModel)
    // if Φ contains an empty clause
    } else if (containsEmptyClause(cnf)) {
      println("Empty Clause")
      None
    // Φ ← unit-propagate(l, Φ);      
    } else if (firstUnitClause(cnf).isDefined) {
      // println("Unit-Propagate (" + firstUnitClause(cnf).get + ")")
      DPLLaux(assignLiteral(cnf, firstUnitClause(cnf).get.head), firstUnitClause(cnf).get.head :: partialModel)
    // Φ ← pure-literal-assign(l, Φ);      
    } else if (firstPureLiteral(cnf).isDefined) {
      // println("Pure Literal (" + firstPureLiteral(cnf).get + ")")
      DPLLaux(assignLiteral(cnf, firstPureLiteral(cnf).get), firstPureLiteral(cnf).get :: partialModel)
    // return DPLL(Φ ∧ l) or DPLL(Φ ∧ not(l));
    } else {
      println("Split")
      val sub = DPLLaux(assignLiteral(cnf, firstLiteral(cnf)), firstLiteral(cnf) :: partialModel)
      if (sub.isDefined) sub else { backtracks += 1 ; DPLLaux(assignLiteral(cnf, -firstLiteral(cnf)), -firstLiteral(cnf) :: partialModel) }
    }
  }

  def DPLL(cnf : CNF) = DPLLaux(cnf, List())

  def verifyModel(cnf : CNF, model : Model) = {
    for (c <- cnf) {
      println(c + " => " + c.find(model contains _).get)
    }
  }

  def main(args : Array[String]) = {
    println("Welcome to scalaSAT")
    if (args.length < 1) {
      println("Usage: scalaSAT input")
    } else {
      val cnf = parseFile(args(0))
      println(cnf)
      val model = DPLL(cnf)
      println("DPLL: "+ model.get.sortWith(Math.abs(_) < Math.abs(_)))
      println("\tBacktracks: " + backtracks)
      if (model.isDefined)
        verifyModel(cnf, model.get)
    }
  }
}
