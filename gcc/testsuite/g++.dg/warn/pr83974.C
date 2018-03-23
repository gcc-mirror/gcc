// { dg-options "-Wtautological-compare" }

struct A {
  typedef void (A::*B) ();
  operator B ();
};
template <typename>
struct C {
  void foo () { d == 0; }
  A d;
};
