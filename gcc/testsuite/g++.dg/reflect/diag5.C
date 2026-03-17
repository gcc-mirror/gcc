// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we decompose reflections in static_asserts, where applicable.

constexpr auto a = ^^int;
consteval auto b() { return ^^double; }

static_assert(a == b());  // { dg-error "static assertion failed" }
// { dg-message "the comparison reduces to '\\\(\\^\\^int == \\^\\^double\\\)'" "" { target *-*-* } .-1 }
