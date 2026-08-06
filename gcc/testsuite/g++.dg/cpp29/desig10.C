// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int a, b, c; };
struct B { int d, e; };
struct C : A { int f, g; };
struct D : B { int h, i; };
struct E : C, D { int j; };
constexpr auto e1 = E { .a = 1, .c = 2, .f = 3, .d = 4, .i = 5, .j = 6 };
static_assert (e1.a == 1 && e1.b == 0 && e1.c == 2 && e1.f == 3 && e1.g == 0
	       && e1.d == 4 && e1.e == 0 && e1.h == 0 && e1.i == 5 && e1.j == 6);
auto e2 = E { 1, 2, 3, 4, 5, .j = 6 };	// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" }
constexpr auto e3 = E { { .a = 1, .b = 2 }, .j = 3 };
static_assert (e3.a == 1 && e3.b == 2 && e3.c == 0 && e3.f == 0 && e3.g == 0
	       && e3.d == 0 && e3.e == 0 && e3.h == 0 && e3.i == 0 && e3.j == 3);
constexpr auto e4 = E { {}, { { .e = 1 }, .i = 2 }, .j = 3 };
static_assert (e4.a == 0 && e4.b == 0 && e4.c == 0 && e4.f == 0 && e4.g == 0
	       && e4.d == 0 && e4.e == 1 && e4.h == 0 && e4.i == 2 && e4.j == 3);
constexpr auto e5 = E { { { .b = 1 }, .f = 2 }, { { .d = 3 }, .h = 4 }, .j = 5 };
static_assert (e5.a == 0 && e5.b == 1 && e5.c == 0 && e5.f == 2 && e5.g == 0
	       && e5.d == 3 && e5.e == 0 && e5.h == 4 && e5.i == 0 && e5.j == 5);
constexpr auto e6 = E { .a = 1, .b = 2, .c = 3, .f = 4, .g = 5, .d = 6,
			.e = 7, .h = 8, .i = 9, .j = 10 };
static_assert (e6.a == 1 && e6.b == 2 && e6.c == 3 && e6.f == 4 && e6.g == 5
	       && e6.d == 6 && e6.e == 7 && e6.h == 8 && e6.i == 9 && e6.j == 10);
auto e7 = E { .a = 1, .b = 2, .c = 3, .d = 4, .e = 5, .f = 6, .g = 7,
	      .h = 8, .i = 9, .j = 10 };// { dg-error "designator order for field 'E::C' does not match declaration order in 'E'" }
auto e8 = E { {}, {}, .a = 1, .j = 2 };	// { dg-error "designator order for field 'E::C' does not match declaration order in 'E'" }
auto e9 = E { {}, .f = 1 };		// { dg-error "designator order for field 'E::C' does not match declaration order in 'E'" }
auto e10 = E { {}, {}, .e = 1 };	// { dg-error "designator order for field 'E::D' does not match declaration order in 'E'" }
auto e11 = E { {}, {}, .h = 1 };	// { dg-error "designator order for field 'E::D' does not match declaration order in 'E'" }
auto e12 = E { .a = 0, .b = 0, .c = 0, .d = 0, .e = 0, .f = 0, .g = 0, .h = 0, .i = 0, .j = 0,
	       .a = 0, .b = 0, .c = 0, .d = 0, .e = 0, .f = 0, .g = 0, .h = 0, .i = 0, .j = 0 };
// { dg-error "'.a' designator used multiple times in the same initializer list" "" { target *-*-* } .-1 }
// { dg-error "'.b' designator used multiple times in the same initializer list" "" { target *-*-* } .-2 }
// { dg-error "'.c' designator used multiple times in the same initializer list" "" { target *-*-* } .-3 }
// { dg-error "'.d' designator used multiple times in the same initializer list" "" { target *-*-* } .-4 }
// { dg-error "'.e' designator used multiple times in the same initializer list" "" { target *-*-* } .-5 }
// { dg-error "'.f' designator used multiple times in the same initializer list" "" { target *-*-* } .-6 }
// { dg-error "'.g' designator used multiple times in the same initializer list" "" { target *-*-* } .-7 }
// { dg-error "'.h' designator used multiple times in the same initializer list" "" { target *-*-* } .-8 }
// { dg-error "'.i' designator used multiple times in the same initializer list" "" { target *-*-* } .-9 }
// { dg-error "'.j' designator used multiple times in the same initializer list" "" { target *-*-* } .-10 }
