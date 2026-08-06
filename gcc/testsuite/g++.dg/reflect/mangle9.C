// { dg-do compile { target c++26 } }
// { dg-options "-freflection -O0" }

#include <meta>

struct W { union { union {}; union {}; union {}; union {}; };
	   union { union {}; union {}; union {}; union {}; }; };

constexpr auto ctx = std::meta::access_context::current ();

template <int N, std::meta::info I>
void
bar ()
{
}

void
baz (int x)
{
  constexpr auto a = members_of (^^W, ctx)[0];
  bar <10, members_of (a, ctx)[1]> (); // empty anon union non-static data member
  bar <11, members_of (a, ctx)[7]> (); // empty anon union non-static data member
  constexpr auto b = members_of (^^W, ctx)[2];
  bar <12, members_of (b, ctx)[1]> (); // empty anon union non-static data member
  bar <13, members_of (b, ctx)[7]> (); // empty anon union non-static data member
}

// { dg-final { scan-assembler "_Z3barILi10ELDmda1WUt__EEvv" } }
// { dg-final { scan-assembler "_Z3barILi11ELDmda1WUt_2_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi12ELDmda1WUt0__EEvv" } }
// { dg-final { scan-assembler "_Z3barILi13ELDmda1WUt0_2_EEvv" } }
