struct SPxPricer;
struct SoPlex {
  virtual void setPricer(SPxPricer *);
};
struct SPxPricer {
  virtual void load(SoPlex *);
};
struct SLUFactor {
  SLUFactor();
  virtual ~SLUFactor();
};
struct SPxSolver : SoPlex {
  SPxPricer pr;
  SLUFactor slu;
  SPxSolver();
};
SPxSolver::SPxSolver() { setPricer(&pr); }
