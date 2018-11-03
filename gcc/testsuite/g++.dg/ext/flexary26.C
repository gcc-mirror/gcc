// PR c++/81922
// { dg-do compile }
// { dg-options "-Wpedantic" }

struct S { const char *a; char b[]; };	// { dg-warning "32:ISO C\\+\\+ forbids flexible array member" }
struct T { int a; int b[]; };	// { dg-warning "23:ISO C\\+\\+ forbids flexible array member" }
#if __cplusplus >= 201103L
S c[] { "", "" };		// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
				// { dg-warning "initialization of a flexible array member" "" { target c++11 } .-1 }
S d[] { "", { 0 } };		// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
				// { dg-warning "initialization of a flexible array member" "" { target c++11 } .-1 }
T e[] { 1, { 2 }, 3, { 4 } };	// { dg-error "initialization of flexible array member in a nested context" "" { target c++11 } }
				// { dg-warning "initialization of a flexible array member" "" { target c++11 } .-1 }
T f[] { 1, {}, 3, {} };		// { dg-warning "initialization of a flexible array member" "" { target c++11 } }
T g { 1, { 1, 2, 3 } };		// { dg-warning "initialization of a flexible array member" "" { target c++11 } }
S h { "abcd", "" };		// { dg-warning "initialization of a flexible array member" "" { target c++11 } }
#endif
S i[] = { "", "", "", "" };	// { dg-error "initialization of flexible array member in a nested context" }
				// { dg-warning "initialization of a flexible array member" "" { target *-*-* } .-1 }
S j[] = { "", { 1 }, "", { 2, 3 } };	// { dg-error "initialization of flexible array member in a nested context" }
				// { dg-warning "initialization of a flexible array member" "" { target *-*-* } .-1 }
T k[] = { 1, { 2 }, 3, { 4 } };	// { dg-error "initialization of flexible array member in a nested context" }
				// { dg-warning "initialization of a flexible array member" "" { target *-*-* } .-1 }
T l[] = { 1, {}, 3, {} };	// { dg-warning "initialization of a flexible array member" }
T m = { 1, { 1, 2, 3 } };	// { dg-warning "initialization of a flexible array member" }
S n = { "", "abcde" };		// { dg-warning "initialization of a flexible array member" }
