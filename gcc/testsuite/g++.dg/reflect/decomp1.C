// PR c++/126680
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <array>

template<int = 0>
void f ()
{
  static constexpr auto [...a] = std::array { ^^int };
}

void
g ()
{
  f ();
}
