// PR c++/58888
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A
{
  static constexpr auto b{1.0};
};

constexpr decltype(A::b) A::b;
