// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test null reflection.

using info = decltype(^^int);
consteval info foo () { return {}; }
static_assert (foo () == info ());
consteval auto bar () { return info{}; }
static_assert (bar () == info ());
