// { dg-do assemble  }
// GROUPS passed niklas static-members
struct A { static A a; };
A f () { return A::a; }
