// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct X { constexpr operator std::meta::info(); }; // { dg-error "function of consteval-only type" }
// { dg-warning "used but never defined" "" { target *-*-* } .-1 }
constexpr auto r = std::meta::type_of (X{}); // { dg-error "used before its definition" }
