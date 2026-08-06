// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }
// { dg-options "-Wmissing-braces" }

struct A { int a, b; };
struct B { int c, d; };
struct C : A, B { int e, f; };
struct D : C { int g; };
auto c1 = C { { .a = 1, .b = 2 }, { .c = 3, .d = 4 }, .e = 5, .f = 6 };	// { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto c2 = C { { .a = 1, .b = 2 }, .e = 5, .f = 6 };			// { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto c3 = C { {}, { .c = 3, .d = 4 }, .f = 6 };				// { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto c4 = C { .e = 1, 2 };						// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
									// { dg-error "designated initializer clause should not be followed by non-designated" "" { target c++29 } .-1 }
auto c5 = C { .a = 1, .b = 2, .c = 3, .d = 4, .e = 5, .f = 6 };		// { dg-error "'C' has no non-static data member named 'a'" "" { target c++26_down } }
									// { dg-warning "missing braces around initializer for 'A'" "" { target c++29 } .-1 }
									// { dg-warning "missing braces around initializer for 'B'" "" { target c++29 } .-2 }
auto a1 = A { 1, .b = 2 };						// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" }
									// { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } .-1 }
auto d1 = D { .a = 1, .b = 2, .c = 3, .d = 4, .e = 5, .f = 6, .g = 7 };	// { dg-error "'D' has no non-static data member named 'a'" "" { target c++26_down } }
									// { dg-warning "missing braces around initializer for 'A'" "" { target c++29 } .-1 }
									// { dg-warning "missing braces around initializer for 'B'" "" { target c++29 } .-2 }
									// { dg-warning "missing braces around initializer for 'C'" "" { target c++29 } .-3 }
auto d2 = D { { { .a = 1, .b = 2 }, { .c = 3, .d = 4 }, .e = 5, .f = 6 }, .g = 7 }; // { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
