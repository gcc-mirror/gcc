// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "_ZN1BC2Ev" } }
// { dg-final { scan-assembler-not "_ZN1BD2Ev" } }

struct A { int i; A(); virtual ~A() = 0; };
struct B final: public virtual A { int j; B(); ~B(); };

B::B() {}
B::~B() {}
