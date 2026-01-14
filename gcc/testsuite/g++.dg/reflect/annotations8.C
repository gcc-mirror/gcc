// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct [[=42]] A {};
template <auto I>
inline auto b = [] (auto &&x) { x.template operator () <I> (); };

int
main ()
{
  auto l = [&] <auto> {};
  auto w = b <annotations_of (^^A)[0]>;
  w (l);
}
