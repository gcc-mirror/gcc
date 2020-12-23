// PR c++/79294

template <int()> struct a;
template <int(b)> a < b		// { dg-error "int" }
// { dg-error "expected" "" { target *-*-* } .-1 }
