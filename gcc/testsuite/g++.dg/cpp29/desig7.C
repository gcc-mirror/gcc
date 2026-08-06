// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int a; };
struct B : A { int b; };
struct C : B { int c; };
constexpr B x = B { .a = 1 };
static_assert (x.a == 1 && x.b == 0);
constexpr C y = C { .a = 2, .b = 3, .c = 4 };
static_assert (y.a == 2 && y.b == 3 && y.c == 4);
