// { dg-do compile { target c++20 } }

#include "construct_at.h"

struct S { int x; };

constexpr bool foo(S s, S*& p) {
  p = &s;
  s.~S();
  return true;
}

constexpr bool bar() {
  // This is, strictly speaking, implementation-defined behaviour;
  // see [expr.call] p6.  However, in all other cases we destroy
  // at the end of the full-expression, so the below should be fixed.
  S* p;
  foo(S{}, p), std::construct_at(p);  // { dg-bogus "destroying" "" { xfail *-*-* } }

  return true;
}

constexpr bool x = bar();
