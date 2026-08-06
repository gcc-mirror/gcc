// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++29 } }

struct A { int a1, a2; };
struct B : A { int b; };
struct C : A { int a1; };
A v0 = A { 1, .a2 = 2 };			// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" }
constexpr B v1 = B { .a1 = 1, .b = 2 };		// the explicitly initialized elements are [A, B::b]
static_assert (v1.a1 == 1 && v1.a2 == 0 && v1.b == 2);
constexpr B v2 = B { .a1 = 1, .a2 = 2, .b = 3 };// the explicitly initialized elements are [A, B::b]
static_assert (v2.a1 == 1 && v2.a2 == 2 && v2.b == 3);
constexpr B v3 = B { A { 1, 2 }, .b = 3 };	// the explicitly initialized elements are [A, B::b]
static_assert (v3.a1 == 1 && v3.a2 == 2 && v3.b == 3);
B v4 = B { A { }, .a2 = 1, .b = 3 };		// { dg-error "designator order for field 'B::A' does not match declaration order in 'B'" }
constexpr C v5 = C { .a1 = 4 };			// the explicitly initialized elements are [C::a1]
static_assert (v5.A::a1 == 0 && v5.a2 == 0 && v5.C::a1 == 4);
