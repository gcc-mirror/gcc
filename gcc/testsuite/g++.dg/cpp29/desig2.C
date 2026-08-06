// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }

struct A { A (const char *); int a; };
struct B : A { int b, c; };
auto b1 = B { "hello", .b = 3, .c = 4 };	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto b2 = B { { "hello" }, .b = 3, .c = 4 };	// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto b3 = B { "nope", 3, .c = 4 };		// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" }
						// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } .-1 }
