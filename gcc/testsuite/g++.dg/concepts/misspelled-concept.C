// { dg-do compile { target c++20 } }
// { dg-options "-fconcepts" }

#include <concepts>

template <std::unsinged_integral N> foo (N); // { dg-error "'std::unsinged_integral' has not been declared; did you mean 'std::unsigned_integral'" }
// { dg-error "expected" "followup" { target *-*-* } .-1 }
