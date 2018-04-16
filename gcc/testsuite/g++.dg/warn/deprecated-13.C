// PR c++/84222
// { dg-do compile }

struct __attribute__((deprecated)) C {		// { dg-message "declared here" }
  C () {}
  C (const C &);				// { dg-bogus "'C' is deprecated" }
  C (const C &x, const C &y) { C z = x; }	// { dg-bogus "'C' is deprecated" }
  void foo (const C &x, const C &y);		// { dg-bogus "'C' is deprecated" }
};

void
C::foo (const C &x, const C &y)			// { dg-bogus "'C' is deprecated" }
{
  C z = x;					// { dg-bogus "'C' is deprecated" }
}

void
bar (const C &x, const C &y)			// { dg-warning "'C' is deprecated" }
{
  C z = x;					// { dg-warning "'C' is deprecated" }
}

template <int N>
struct __attribute__((deprecated)) D {		// { dg-message "declared here" }
  D () {}
  D (const D &);				// { dg-bogus "is deprecated" }
  D (const D &x, const D &y) { D z = x; }	// { dg-bogus "is deprecated" }
  void foo (const D &x, const D &y);		// { dg-bogus "is deprecated" }
};

template <int N>
void
D<N>::foo					// { dg-bogus "is deprecated" "" { xfail *-*-* } }
(const D &x, const D &y)			// { dg-bogus "is deprecated" }
{
  D z = x;					// { dg-bogus "is deprecated" }
}

template <int N>
void
bar (const D<N> &x, const D<N> &y)		// { dg-warning "is deprecated" }
{
  D<N> z = x;					// { dg-warning "is deprecated" }
}
