// Bug: the synthesized copy constructor for A is not found.
// Build don't link:

struct A {
  // A (const A& a): i(a.i) {}
  int i;
};

struct B {
  A a;
  B (const B& b): a(b.a), j(b.j) { } // gets bogus error - 
  int j;
};
