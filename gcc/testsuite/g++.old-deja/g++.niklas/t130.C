// { dg-do assemble  }
// GROUPS passed niklas nested-types
struct A;
struct B { struct A { A(int); }; struct C : A { C() : A (0) {} }; };
