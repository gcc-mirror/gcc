// Build don't link:

// Simplified from bug report by Tim Rowley <tor@cs.brown.edu>

struct baz;

void operator*(baz&, double);

template <class T> inline T operator*(double s, const T &p)
  ; // gets bogus error - must have argument of class type - XFAIL *-*-*

void m(baz& a) { a * .5; }
