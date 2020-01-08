// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++17 } }
// Test typeid in a template.

#include <typeinfo>

struct B { virtual void f(); };
struct B2 : B { };

template<typename T>
constexpr bool
fn ()
{
  constexpr B2 b2;
  static_assert(&typeid(b2) == &typeid(B2)); // { dg-error ".typeid. is not a constant expression because .b2. is of polymorphic type|non-constant condition" "" { target c++17_down } }
  return true;
}

static_assert (fn<int>());
