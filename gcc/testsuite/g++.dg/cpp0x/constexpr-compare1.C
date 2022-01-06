// { dg-do compile { target c++11 } }

extern int a, b;
static_assert (&a == &a, "");
static_assert (&a != &b, "");
constexpr bool c = &a == &a;
constexpr bool d = &a != &b;
