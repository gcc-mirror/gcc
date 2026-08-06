// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int a; };
struct B : A { int b; };
constexpr auto b1 = B { { 1 }, 2 };
constexpr auto b2 = B { 3, 4 };
constexpr auto b3 = B { .a = 5, .b = 6 };
constexpr auto b4 = B { { .a = 7 }, .b = 8 };
constexpr auto b5 = B { .a { 9 }, .b { 10 } };
static_assert (b1.a == 1 && b1.b == 2);
static_assert (b2.a == 3 && b2.b == 4);
static_assert (b3.a == 5 && b3.b == 6);
static_assert (b4.a == 7 && b4.b == 8);
static_assert (b5.a == 9 && b5.b == 10);

struct C { constexpr C (const char *x) : a (0) { while (*x) a += *x++; } int a; };
struct D : C { int b, c; };
constexpr auto d1 = D { "abc", .b = 3, .c = 4 };
constexpr auto d2 = D { { "de" }, .b = 5, .c = 6 };
static_assert (d1.a == 'a' + 'b' + 'c' && d1.b == 3 && d1.c == 4);
static_assert (d2.a == 'd' + 'e' && d2.b == 5 && d2.c == 6);

struct E { int x; };
struct F : E { int x; };
constexpr auto f1 = F { .x = 1 };
constexpr auto f2 = F { { .x = 2 }, .x = 3 };
constexpr auto f3 = F { E { 4 }, .x = 5 };
static_assert (f1.E::x == 0 && f1.F::x == 1);
static_assert (f2.E::x == 2 && f2.F::x == 3);
static_assert (f3.E::x == 4 && f3.F::x == 5);

struct G { int g; };
struct H { int h; };
struct I : G, H { int i; };
constexpr auto i1 = I { { .g = 1 }, { .h = 2 }, .i = 3 };
constexpr auto i2 = I { { .g = 4 }, .h = 5, .i = 6 };
constexpr auto i3 = I { { .g = 7 }, H { 8 }, .i = 9 };
static_assert (i1.g == 1 && i1.h == 2 && i1.i == 3);
static_assert (i2.g == 4 && i2.h == 5 && i2.i == 6);
static_assert (i3.g == 7 && i3.h == 8 && i3.i == 9);
