// Test for mangling of simple testcase involving construction vtables.

// { dg-do compile }

struct A {
  virtual void f () { }
};

struct B: public virtual A { };
struct C: public B { };

C c;

// { dg-final { scan-assembler mangle1.C "\n_ZN1A1fEv:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZN1AC2Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZN1BC2Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZN1CC1Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTC1C0_1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTI1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTI1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTI1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTS1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTS1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTS1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTT1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTV1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTV1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_ZTv0_n12_N1A1fEv:" } }
