// Test for mangling of simple testcase involving construction vtables.

// { dg-do compile }
// { dg-options "-fno-inline" }

struct A {
  virtual void f () { }
};

struct B: public virtual A { };
struct C: public B { };

C c;

// { dg-final { scan-assembler mangle1.C "\n_?_ZN1A1fEv:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZN1AC2Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZN1BC2Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZN1CC1Ev:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTC1C0_1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTI1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTI1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTI1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTS1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTS1B:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTS1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTT1C:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTV1A:" } }
// { dg-final { scan-assembler mangle1.C "\n_?_ZTV1C:" } }
