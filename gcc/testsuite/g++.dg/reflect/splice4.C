// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

namespace M
{
  template <int N>
  constexpr int foo () { return N; }
}

static_assert (template [: ^^M::foo :] <42> () == 42);
static_assert (template [: members_of (^^M, std::meta::access_context::unchecked ())[0] :] <43> () == 43);
int a = [: ^^M::foo :] <44> ();
// { dg-error "reflection 'M::foo<44>' not usable in a splice expression with template arguments" "" { target *-*-* } .-1 }
int b = [: members_of (^^M, std::meta::access_context::unchecked ())[0] :] <45> ();
// { dg-error "reflection 'M::foo<45>' not usable in a splice expression with template arguments" "" { target *-*-* } .-1 }
