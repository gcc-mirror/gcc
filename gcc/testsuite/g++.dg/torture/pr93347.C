// { dg-additional-options "--param early-inlining-insns=3 --param ipa-cp-eval-threshold=100" }


struct A {
  int a;
};
struct B {
  int b;
};
struct C : B, A {};
struct RA {
  virtual A *operator-();
};
struct RC : RA {
  C *operator-() {
    C *x = new C();
    return x;
  }
};
void fop(RC *x) { -static_cast<RA &>(*x); }
