// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_explicit_object_parameter.

#include <meta>

using namespace std::meta;

struct S {
  void f (this S, int);
};

constexpr auto p = parameters_of (^^S::f)[0];

void S::f (this S, int) {}

static_assert (is_explicit_object_parameter (p));
