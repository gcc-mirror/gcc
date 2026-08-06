// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }

struct A { int a; };
struct B : A { int b; };
auto b1 = B { { 1 }, 2 };
auto b2 = B { 1, 2 };
auto b3 = B { .a = 1, .b = 2 };		// { dg-error "'B' has no non-static data member named 'a'" "" { target c++26_down } }
auto b4 = B { { .a = 1 }, .b = 2 };	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto b5 = B { .a { 1 }, .b { 2 } };	// { dg-error "'B' has no non-static data member named 'a'" "" { target c++26_down } }
auto b6 = B { .b = 2, .a = 1 };		// { dg-error "designator order for field 'B::A' does not match declaration order in 'B'" "" { target c++29 } }
					// { dg-error "'B' has no non-static data member named 'a'" "" { target c++26_down } .-1 }
