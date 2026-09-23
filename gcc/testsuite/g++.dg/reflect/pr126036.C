// PR c++/126036
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

namespace A { int a; };

template <int N>
void
foo ()
{
  constexpr auto m = std::meta::members_of (^^A, std::meta::access_context::unchecked ());
  // { dg-error "is not a constant expression because it refers to a result of 'operator new'" "" { target *-*-* } .-1 }
}

void
bar ()
{
  foo <42> ();
}
