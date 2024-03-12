// CWG 2799
// Test that inheriting a trivial default constructor produces a trivial
// default constructor.

// { dg-do compile { target c++11 } }

#include <type_traits>

struct A {
  A() = default;
};

struct B : A
{
  using A::A;
  B(int);
};

static_assert (std::is_trivially_constructible<B>::value, "");
