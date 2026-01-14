// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Per N5014 [temp.arg.nontype] copying an template
// parameter object of class type must produce structurally
// equivalent values, but both clang and GCC does not seem
// to implement that.

#include <meta>

using namespace std::meta;

struct A {
  int x;
  constexpr A(int p) : x(p) {}
  constexpr A(const A &oth) : x(oth.x ? oth.x-1 : oth.x) {
  }
};

consteval bool
can_reflect_constant (int x)
{
  try { reflect_constant (A(x)); }
  catch (std::meta::exception &) { return false; }
  return true;
}

// FIXME
// This should pass, with A::x being zero, copies are equivalent.
static_assert(!can_reflect_constant(0));
// As neither GCC and clang see to implement the aforementioned
// rule, I believe all should work.
static_assert(!can_reflect_constant(1));
static_assert(!can_reflect_constant(4));
