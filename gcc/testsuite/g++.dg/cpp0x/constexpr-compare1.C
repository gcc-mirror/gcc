// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

extern int a, b;
static_assert (&a == &a, "");
static_assert (&a != &b, "");
constexpr bool c = &a == &a;
constexpr bool d = &a != &b;
