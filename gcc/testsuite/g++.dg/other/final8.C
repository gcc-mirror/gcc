// PR c++/70462
// PR c++/95428
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1BC1Ev" } }
// { dg-final { scan-assembler "_ZN1BC2Ev" } }
// { dg-final { scan-assembler "_ZN1BD2Ev" } }
// { dg-final { scan-assembler "_ZN1BD1Ev" } }

struct A { int i; A(); virtual ~A() = 0; };
struct B final: public virtual A { int j; B(); ~B(); };

B::B() {}
B::~B() {}
