// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we suggest adding "typename" (where allowed).

constexpr auto r = ^^int;

template<typename> struct S {};

void
g ()
{
  [:r:] i = 42;  // { dg-error "expected a reflection of an expression" }
// { dg-message "add .typename. to denote a type outside a type-only context" "" { target *-*-* } .-1 }

  S<([:r:])> s;  // { dg-error "expected a reflection of an expression|template argument 1 is invalid" }
// { dg-message "add .typename. to denote a type outside a type-only context" "" { target *-*-* } .-1 }
}
