// PR c++/92070 - bogus error with -fchecking=2.
// { dg-additional-options "-fchecking=2" }

struct a;
struct b {
  static a c();
};
struct a : b {};
template <class> struct d {
  void e() { 0 ? b() : b::c(); }
};
