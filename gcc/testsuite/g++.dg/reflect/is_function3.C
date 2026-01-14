// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_function.

#include <meta>

using namespace std::meta;

struct S {
  S();
  ~S();
};

static_assert (is_function (^^S::S)); // { dg-error "cannot take the reflection of an overload set" }
static_assert (is_function (^^S::~S));
