// PR c++/51577
// Verify we don't consider later-declared namespace-scope operator overloads
// when instantiating a dependent operator expression that occurs at block scope.

template <class T> void f (T x) {
  +x; // { dg-error "no match" }
  -x; // { dg-error "no match" }
  *x; // { dg-error "no match" }
  ~x; // { dg-error "no match" }
  &x;
  !x; // { dg-error "no match" }
  ++x; // { dg-error "no match" }
  --x; // { dg-error "no match" }
  x++; // { dg-error "declared for postfix" }
  x--; // { dg-error "declared for postfix" }

  x->*x; // { dg-error "no match" }
  x / x; // { dg-error "no match" }
  x * x; // { dg-error "no match" }
  x + x; // { dg-error "no match" }
  x - x; // { dg-error "no match" }
  x % x; // { dg-error "no match" }
  x & x; // { dg-error "no match" }
  x | x; // { dg-error "no match" }
  x ^ x; // { dg-error "no match" }
  x << x; // { dg-error "no match" }
  x >> x; // { dg-error "no match" }
  x && x; // { dg-error "no match" }
  x || x; // { dg-error "no match" }
  x, x;

  x == x; // { dg-error "no match" }
  x != x; // { dg-error "no match" }
  x < x; // { dg-error "no match" }
  x > x; // { dg-error "no match" }
  x <= x; // { dg-error "no match" }
  x >= x; // { dg-error "no match" }
#if __cplusplus > 201703L
  x <=> x; // { dg-error "no match" "" { target c++20 } }
#endif

  x += x; // { dg-error "no match" }
  x -= x; // { dg-error "no match" }
  x *= x; // { dg-error "no match" }
  x /= x; // { dg-error "no match" }
  x %= x; // { dg-error "no match" }
  x |= x; // { dg-error "no match" }
  x ^= x; // { dg-error "no match" }
  x <<= x; // { dg-error "no match" }
  x >>= x; // { dg-error "no match" }
}

namespace N { struct A { }; }

#include "operator-3-ops.h"

int main() {
  f(N::A());
}
