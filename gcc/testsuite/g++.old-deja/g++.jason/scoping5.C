// Bug: g++ thinks that A defines operator delete, and tries to call it.
// Build don't link:

struct A {
  ~A () { ::operator delete (0); }
};
