// { dg-do assemble  }
// GROUPS passed niklas explicit-construct
struct A { A(); };
struct B { B(A&); };
B b(A());
