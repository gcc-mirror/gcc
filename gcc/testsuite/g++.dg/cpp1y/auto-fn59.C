// PR c++/79815
// { dg-do compile { target c++14 } }

decltype(auto) const x = 1; // { dg-error "cannot be cv-qualified" }
volatile decltype(auto) x2 = 1; // { dg-error "cannot be cv-qualified" }
const volatile decltype(auto) x3 = 1; // { dg-error "cannot be cv-qualified" }
const decltype(auto) fn() { return 42; } // { dg-error "cannot be cv-qualified" }
const decltype(auto) fn2(); // { dg-error "cannot be cv-qualified" }
