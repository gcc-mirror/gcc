// { dg-do assemble  }
// the template operator!= interferes.  It should be in a namespace.

#include <utility>

enum T {
  V1
};

struct X {
  T      t : 31;
};

void
f(X& v) {
  if( v.t != V1 ) {		// complains about taking address of bitfield
  }
}
