// { dg-do compile { target c++20 } }
// { dg-options "-fconcepts" }

template <std::integral N> foo (N); // { dg-error "'std::integral' has not been declared" }
// { dg-message "'std::integral' is defined in header '<concepts>'" "note" { target *-*-* } .-1 }
// { dg-error "expected" "followup" { target *-*-* } .-2 }
