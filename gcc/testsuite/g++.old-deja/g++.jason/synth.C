// { dg-do assemble  }
// Bug: the synthesized copy constructor for A is not found.

struct A {
  // A (const A& a): i(a.i) {}
  int i;
};

struct B {
  A a;
  B (const B& b): a(b.a), j(b.j) { } // { dg-bogus "" } 
  int j;
};
