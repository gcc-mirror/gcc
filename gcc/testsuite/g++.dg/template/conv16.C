// PR c++/95508
// { dg-do compile }

template <typename>
struct A;
template <typename>
struct B {
  operator int () { return 0; }
};
template <>
struct A<unsigned> : B<int> {};
struct D {
  template <typename>
  int foo () { return e[f]; }
  int e[6];
  A<unsigned> f;
};
