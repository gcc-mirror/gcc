// PR c++/51577
// { dg-do compile { target c++14 } }
// Like operator-3.C but also containing a partial instantiation step.

template <class...> auto f () {
  return [] (auto x) {
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
  };
}

namespace N { struct A { }; }

#include "operator-3-ops.h"

int main() {
  f()(N::A());
}
