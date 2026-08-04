// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

struct V { int a, b, c; };
constexpr info varr[] = { ^^V::a, ^^V::b, ^^V::c };
constexpr const info *p = varr;
static_assert (p[0] == ^^V::a);
static_assert (p[1] == ^^V::b);
static_assert (p[2] == ^^V::c);
static_assert (varr[0] == ^^V::a);

constexpr info arr[] = { ^^int, ^^double };
auto q = &arr[0]; // { dg-error ".q. is initialized with a consteval-only value but is not declared .constexpr." }

void
fn ()
{
  auto r = arr; // { dg-error ".r. is initialized with a consteval-only value but is not declared .constexpr." }
  (void) r;
}
