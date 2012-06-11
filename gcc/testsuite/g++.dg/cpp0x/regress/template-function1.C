// PR c++/38647
// { dg-do compile }
// { dg-options "-std=c++0x" }
// { dg-prune-output "note" }

template<const char *, int> struct A {};
const char func[] = "abc";
template<int N> struct A<func, N> {};	// { dg-error "cannot appear|is invalid|not a valid|constant expression" }

char a1[1];
A<a1, 0> a;

template<const char *, int> struct B {};
template<int N> struct B<__FUNCTION__, N> {};	// { dg-error "cannot appear|is invalid|is not a valid|constant expression" }

char b1[1];
B<b1, 0> b;

template<const char *, int> struct C {};
template<int N> struct C<__PRETTY_FUNCTION__, N> {};	// { dg-error "cannot appear|is invalid|is not a valid|constant expression" }

char c1[1];
C<c1, 0> c;

template<const char *, int> struct D {};
template<int N> struct D<__func__, N> {};	// { dg-error "(cannot appear|is invalid|is not a valid|function scope|constant expression)" }
						// { dg-warning "function scope" "function scope" { target *-*-* } 26 }

char d1[1];
D<d1, 0> d;
