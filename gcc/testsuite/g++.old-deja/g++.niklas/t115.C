// { dg-do assemble  }
// GROUPS passed niklas virtual-base
struct A { A(); virtual void f(); };
struct B : virtual A { B(); };
struct C : B {};
C *c = new C;

