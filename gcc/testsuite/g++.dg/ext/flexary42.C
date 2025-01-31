// PR c++/117516
// { dg-do compile }
// { dg-options "-Wpedantic -Wno-error=pedantic" }

struct A { int a; char b[]; int c; };						// { dg-warning "forbids flexible array member" }
										// { dg-error "flexible array member 'A::b' not at end of 'struct A'" "" { target *-*-* } .-1 }
struct B { struct A d; struct A e[]; struct A f; };				// { dg-warning "forbids flexible array member" }
										// { dg-error "flexible array member 'B::e' not at end of 'struct B'" "" { target *-*-* } .-1 }
struct C { struct B g; struct B h[]; struct B i; };				// { dg-warning "forbids flexible array member" }
										// { dg-error "flexible array member 'C::h' not at end of 'struct C'" "" { target *-*-* } .-1 }
struct D { struct C j; };
D k;
struct E { int l; struct { int m; int n[]; int o; } p; int q; };		// { dg-warning "forbids flexible array member" }
										// { dg-error "flexible array member 'E::<unnamed struct>::n' not at end of 'struct E'" "" { target *-*-* } .-1 }
struct F {};
struct G { int r[]; };								// { dg-warning "forbids flexible array member" }
										// { dg-warning "flexible array member 'G::r' in an otherwise empty 'struct G' is a GCC extension" "" { target *-*-* } .-1 }
struct H { int s; int t[]; };							// { dg-warning "forbids flexible array member" }
										// { dg-error "flexible array member 'H::t' not at end of 'struct K'" "" { target *-*-* } .-1 }
										// { dg-message "array member 'int H::t \\\[\\\]' declared here" "" { target *-*-* } .-2 }
#if __cplusplus >= 202002L
struct I { [[no_unique_address]] F u; [[no_unique_address]] F v; int w[]; };	// { dg-warning "forbids flexible array member" "" { target c++20 } }
#endif
struct J { int x; struct H y; };						// { dg-warning "invalid use of 'struct H' with a flexible array member in 'struct J'" }
struct K { int z; struct H aa; int ab; };					// { dg-message "next member 'int K::ab' declared here" }
										// { dg-message "in the definition of 'struct K'" "" { target *-*-* } .-1 }
