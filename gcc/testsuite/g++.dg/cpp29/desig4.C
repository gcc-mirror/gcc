// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }

struct F { int f; };
struct G { int g; };
struct H : F, G { int h; };
auto h1 = H { { .f = 1 }, { .g = 2 }, .h = 3 };		// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto h2 = H { { .f = 1 }, .g = 2, .h = 3 };		// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
							// { dg-error "'H' has no non-static data member named 'g'" "" { target c++26_down } .-1 }
auto h3 = H { { .f = 1 }, G { 2 }, .h = 3 };		// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } }
auto h4 = H { { .f = 1 }, { .g = 2 }, .g = 3, .h = 4 };	// { dg-error "designator order for field 'H::G' does not match declaration order in 'H'" "" { target c++29 } }
							// { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++26_down } .-1 }
							// { dg-error "'H' has no non-static data member named 'g'" "" { target c++26_down } .-2 }
