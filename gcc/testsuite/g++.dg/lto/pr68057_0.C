// { dg-lto-do link }
// { dg-require-effective-target lto_incremental }
/* { dg-extra-ld-options { -O2 -Wno-odr -r -nostdlib } } */
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
struct A : SPxSolver {};
A a;

void SoPlex::setPricer(SPxPricer *p1) { p1->load(this); }

