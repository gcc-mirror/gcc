// PR c++/97878
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern int a[];
auto [b] { a };	// { dg-error "has incomplete type" }
		// { dg-warning "only available with" "" { target c++14_down } .-1 }
auto [c] = a;	// { dg-error "has incomplete type" }
		// { dg-warning "only available with" "" { target c++14_down } .-1 }
extern int d[0];
auto [e] { d };	// { dg-error "too many initializers for" }
		// { dg-error "1 name provided for structured binding" "" { target *-*-* } .-1 }
		// { dg-message "decomposes into 0 elements" "" { target *-*-* } .-2 }
		// { dg-warning "only available with" "" { target c++14_down } .-3 }
auto [f] = d;	// { dg-error "1 name provided for structured binding" }
		// { dg-message "decomposes into 0 elements" "" { target *-*-* } .-1 }
		// { dg-warning "only available with" "" { target c++14_down } .-2 }
