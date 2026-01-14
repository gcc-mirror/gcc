// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Reflection of a reflection.

using info = decltype(^^void);
constexpr info r1 = ^^int;
constexpr info r2 = ^^int;
constexpr info rr1 = ^^r1;
constexpr info rr2 = ^^r2;
static_assert (r1 == ^^int);
static_assert (r1 == r2);
static_assert (r1 != rr1);
static_assert (r1 != rr2);
static_assert (^^r1 != ^^r2);
static_assert (^^int != rr1);
static_assert (^^r1 == rr1);
static_assert (rr2 != rr1);
static_assert (rr2 != ^^r1);
static_assert (rr2 == ^^r2);
static_assert (rr1 != ^^r2);
