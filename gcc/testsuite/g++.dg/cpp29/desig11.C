// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int a, x; };
struct B { int b, x; };
struct C : A, B { int c; };
struct D : C { int x; };
constexpr auto d = D { .c = 1, .x = 2 };
static_assert (d.a == 0 && d.A::x == 0 && d.b == 0 && d.B::x == 0
	       && d.c == 1 && d.D::x == 2);
struct E { int a, x; };
struct F : E { int x; };
constexpr auto f = F { .a = 1, .x = 2 };
static_assert (f.a == 1 && f.E::x == 0 && f.F::x == 2);
struct G { int a, b; };
struct H : G { int c; };
struct I { int a, b, d; };
struct J : I { int e, c; };
constexpr int foo (H, G) { return 1; }
constexpr int foo (J, I) { return 42; }
static_assert (foo ({ .a = 1, .b = 2, .c = 3 }, { .d = 1 }) == 42);
