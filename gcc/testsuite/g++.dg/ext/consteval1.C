// { dg-do compile }
// { dg-options "-std=c++2a" }

consteval int foo (int x) { return x; }
int d = 6;			// { dg-message "'int d' is not const" }
bool e = __builtin_has_attribute (foo (d), packed);	// { dg-error "the value of 'd' is not usable in a constant expression" }
