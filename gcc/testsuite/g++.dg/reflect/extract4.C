// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

struct S {
  int k;
};

// This one verifies that we called mark_used.
constexpr auto a = extract<int S::*>(^^S::k);
static_assert (extract<int S::*>(^^S::k) == &S::k);

consteval int
fn1 ()
{
  return extract<int(*)(int)>(reflect_constant ([](int id) { return id; }))(42);
}
static_assert (fn1 () == 42);

constexpr auto l = [](int id) { return id; };
static_assert (extract<int(*)(int)>(^^l)(42) == 42);
static_assert (extract<decltype(l)>(^^l)(42) == 42);

constexpr auto gl = [](auto a) { return constant_of (a); };
constexpr int i = 42;
static_assert (extract<decltype(gl)>(^^gl)(^^i) == reflect_constant (42));
