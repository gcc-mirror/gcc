// { dg-do assemble  }
// Bug: g++ thinks that A defines operator delete, and tries to call it.

struct A {
  ~A () { ::operator delete (0); }
};
