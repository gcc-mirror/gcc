// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int x; int y; int z; };
A a { .y = 2, .x = 1 };			// { dg-error "designator order for field 'A::x' does not match declaration order in 'A'" }
constexpr A b { .x = 1, .z = 2 };
static_assert (b.x == 1 && b.y == 0 && b.z == 2);
struct B : A { int q; };
constexpr B e { .x = 1, .q = 3 };
static_assert (e.x == 1 && e.y == 0 && e.z == 0 && e.q == 3);
B f { .q = 3, .x = 1 };			// { dg-error "designator order for field 'B::A' does not match declaration order in 'B'" }
struct C { int p; int x; };
struct D : A, C { };
constexpr D g { .y = 1, .p = 2 };
static_assert (g.A::x == 0 && g.y == 1 && g.z == 0 && g.p == 2 && g.C::x == 0);
D h { .x = 2 };				// { dg-error "request for member 'x' is ambiguous" }
struct NonAggr { int na; NonAggr (int); };
struct E : NonAggr { int e; };
E i { .na = 1, .e = 2 };		// { dg-error "designated initializers cannot be used with a non-aggregate type 'NonAggr'" }
