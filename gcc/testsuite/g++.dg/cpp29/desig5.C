// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }

struct A { int a; };
struct B : A { int b; };
struct C : A { C (); int c; };
struct D : C { int d; };

auto a = A { .a = 1 };
auto b = B { .a = 1, .b = 2 };	// { dg-error "'B' has no non-static data member named 'a'" "" { target c++26_down } }
auto c = C { .c = 1 };		// { dg-error "designated initializers cannot be used with a non-aggregate type 'C'" }
				// { dg-error "no matching function for call to" "" { target *-*-* } .-1 }
auto d = D { .a = 1 };		// { dg-error "designated initializers cannot be used with a non-aggregate type 'C'" "" { target c++29 } }
				// { dg-error "'D' has no non-static data member named 'a'" "" { target c++26_down } .-1 }

struct E { int x; };
struct F : E { int x; };
constexpr auto f = F { .x = 1 };
static_assert (f.E::x == 0 && f.F::x == 1);

struct G { int x; };
struct H { int x; };
struct I : G, H { };
auto i = I { .x = 1 };		// { dg-error "request for member 'x' is ambiguous" "" { target c++29 } }
				// { dg-error "'I' has no non-static data member named 'x'" "" { target c++26_down } .-1 }
