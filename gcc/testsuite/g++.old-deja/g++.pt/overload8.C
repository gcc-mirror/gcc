// { dg-do assemble  }

// Simplified from bug report by Tim Rowley <tor@cs.brown.edu>

struct baz;

void operator*(baz&, double);

template <class T> inline T operator*(double s, const T &p)
  ; // { dg-bogus "" "" }  - must have argument of class type - 

void m(baz& a) { a * .5; }
