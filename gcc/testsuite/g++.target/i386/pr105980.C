// { dg-do assemble { target { fpic } } }
// { dg-options "-O0 -fpic -mforce-indirect-call" }

struct A {
  virtual ~A();
};
struct B : virtual A {};
void bar() { B(); }
