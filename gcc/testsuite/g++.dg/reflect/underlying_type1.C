// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::underlying_type.

#include <meta>

constexpr auto a = std::meta::underlying_type (^^int);	// { dg-error "uncaught exception of type" }

enum E {
  E0 = 1,
  E1 = std::meta::underlying_type (^^E) == ^^int,	// { dg-error "uncaught exception of type" }
  E2 = 2						// { dg-error "enumerator value for 'E1' is not an integer constant" "" { target *-*-* } .-1 }
};
