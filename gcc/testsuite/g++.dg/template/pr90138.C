// PR c++/90138

template <, typename T, typename typename, typename T>	// { dg-error "expected" }
struct S;	// { dg-error "no default" }
// { dg-error "two or more" "" { target *-*-* } .-2 }
