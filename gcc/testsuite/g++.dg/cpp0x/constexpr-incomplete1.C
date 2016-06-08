// { dg-do compile { target c++11 } }

struct A
{
  static constexpr A a = 1;  // { dg-error "22:'constexpr const A A::a' has incomplete type" }
  constexpr A(int i) { }
};
