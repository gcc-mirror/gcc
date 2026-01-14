// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Here, x is a null reflection so should be allowed to compare equal
// to itself.  meta::info should be const-default-constructible.

constexpr decltype(^^::) x;
static_assert(x == x);
