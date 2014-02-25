// PR c++/60225
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A() {}
  static constexpr A a[2] = {};  // { dg-error "incomplete" }
};
