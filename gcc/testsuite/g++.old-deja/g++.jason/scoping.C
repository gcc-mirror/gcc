// PRMS Id: 3977
// Bug: A member function is not hidden properly by a later use of its name.
// Build don't link:

struct A {
  void index ();
};

struct B: A {
  int index;
  B(): index(4) {}
};
