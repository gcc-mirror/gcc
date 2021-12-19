// PR c++/51577
// { dg-do compile { target c++17 } }
// Like operator-4.C but also containing a partial instantiation step.

template <class...> auto f () {
  return [] (auto... xs) {
    (xs->*...); // { dg-error "no match" }
    (...->*xs); // { dg-error "no match" }
    (xs / ...); // { dg-error "no match" }
    (... / xs); // { dg-error "no match" }
    (xs * ...); // { dg-error "no match" }
    (... * xs); // { dg-error "no match" }
    (xs + ...); // { dg-error "no match" }
    (... + xs); // { dg-error "no match" }
    (xs - ...); // { dg-error "no match" }
    (... - xs); // { dg-error "no match" }
    (xs % ...); // { dg-error "no match" }
    (... % xs); // { dg-error "no match" }
    (xs & ...); // { dg-error "no match" }
    (... & xs); // { dg-error "no match" }
    (xs | ...); // { dg-error "no match" }
    (... | xs); // { dg-error "no match" }
    (xs ^ ...); // { dg-error "no match" }
    (... ^ xs); // { dg-error "no match" }
    (xs << ...); // { dg-error "no match" }
    (... << xs); // { dg-error "no match" }
    (xs >> ...); // { dg-error "no match" }
    (... >> xs); // { dg-error "no match" }
    (xs && ...); // { dg-error "no match" }
    (... && xs); // { dg-error "no match" }
    (xs || ...); // { dg-error "no match" }
    (... || xs); // { dg-error "no match" }
    (xs, ...);
    (..., xs);

    (xs == ...); // { dg-error "no match" }
    (... == xs); // { dg-error "no match" }
    (xs != ...); // { dg-error "no match" }
    (... != xs); // { dg-error "no match" }
    (xs < ...); // { dg-error "no match" }
    (... < xs); // { dg-error "no match" }
    (xs > ...); // { dg-error "no match" }
    (... > xs); // { dg-error "no match" }
    (xs <= ...); // { dg-error "no match" }
    (... <= xs); // { dg-error "no match" }
    (xs >= ...); // { dg-error "no match" }
    (... >= xs); // { dg-error "no match" }

    (xs += ...); // { dg-error "no match" }
    (... += xs); // { dg-error "no match" }
    (xs -= ...); // { dg-error "no match" }
    (... -= xs); // { dg-error "no match" }
    (xs *= ...); // { dg-error "no match" }
    (... *= xs); // { dg-error "no match" }
    (xs /= ...); // { dg-error "no match" }
    (... /= xs); // { dg-error "no match" }
    (xs %= ...); // { dg-error "no match" }
    (... %= xs); // { dg-error "no match" }
    (xs |= ...); // { dg-error "no match" }
    (... |= xs); // { dg-error "no match" }
    (xs ^= ...); // { dg-error "no match" }
    (... ^= xs); // { dg-error "no match" }
    (xs <<= ...); // { dg-error "no match" }
    (... <<= xs); // { dg-error "no match" }
    (xs >>= ...); // { dg-error "no match" }
    (... >>= xs); // { dg-error "no match" }
  };
}

namespace N { struct A { }; }

#include "operator-3-ops.h"

int main() {
  f()(N::A(), N::A());
}
