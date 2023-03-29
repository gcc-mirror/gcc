// DR 2673 - User-declared spaceship vs. built-in operators
// { dg-do compile { target c++20 } }

#include <compare>

enum class E : int { E1, E2 };
enum class F : int { F1, F2 };

constexpr auto
operator<=> (E lhs, E rhs)
{
  return (int) rhs <=> (int) lhs;
}

constexpr bool
operator== (F lhs, F rhs)
{
  return (int) lhs != (int) rhs;
}

static_assert ((E::E1 <=> E::E2) == (1 <=> 0));
static_assert (E::E1 > E::E2);		// { dg-bogus "static assertion failed" "" { xfail *-*-* } }
static_assert (F::F1 == F::F2);
static_assert (!(F::F1 != F::F2));	// { dg-bogus "static assertion failed" "" { xfail *-*-* } }
