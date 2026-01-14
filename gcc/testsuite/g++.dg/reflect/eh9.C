// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test throwing an exception in a template.

#include <meta>

enum E : int;
template <int K>
constexpr auto R = enumerators_of(^^E)[0];
enum E : int { A, B, C };
static_assert(R<0> == ^^A);
