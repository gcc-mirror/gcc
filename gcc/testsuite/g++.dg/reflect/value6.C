// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

struct S
{
  info i;
  int n;
};

constexpr info arr[] = { ^^int, ^^double, ^^char };
constexpr S s = { ^^int, 42 };
constexpr S sarr[2] = { { ^^int, 1 }, { ^^double, 2 } };
constexpr const info *p = arr;

constexpr const void *vp = &p;

// This tests that consteval_only_value_p properly detects
// consteval-only values when they are in ADDR_EXPR, POINTER_PLUS_EXPR,
// ARRAY_REF, and similar.  A failure would result in a link error.
constexpr const auto p1 = &arr[0];
constexpr const auto p2 = arr;
constexpr const auto p3 = arr + 1;
constexpr const auto p4 = arr + 3;
constexpr const auto p5 = &s;
constexpr const auto p6 = &s.i;
constexpr const auto p7 = &sarr[1];
constexpr const auto p8 = &sarr[1].i;

constexpr bool cond = true;
constexpr auto p_cond = cond ? &arr[0] : &arr[1];

struct Holder { const info *p; int n; };
constexpr Holder h = { &arr[0], 1 };

constexpr auto cpdm = &S::i;

int main () {}
