// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::access_context.

#include <meta>

using namespace std::meta;

template <typename T>
struct S
{
  static constexpr auto ctx = access_context::current ();
};

static_assert (S <int>::ctx.designating_class () == info {} && S <int>::ctx.scope () == ^^S <int>);
