// { dg-do assemble  }
// GROUPS passed niklas nested-types
struct A { struct B { ~B (); }; };
A::B::~B () {}
