// PR c++/60225
// { dg-do compile { target c++11 } }

struct A
{
  constexpr A() {}
  static constexpr A a[2] = {};  // { dg-error "22:elements of array 'constexpr const A A::a \\\[2\\\]' have incomplete type" }
};
