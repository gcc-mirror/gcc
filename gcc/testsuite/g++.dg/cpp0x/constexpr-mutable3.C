// PR c++/92505
// { dg-do compile { target c++11 } }

struct A { mutable int m; };

constexpr int f(A a) { return a.m; }
// { dg-error "mutable" "" { target c++11_only } .-1 }

static_assert(f({42}) == 42, "");
// { dg-error "non-constant" "" { target c++11_only } .-1 }
