// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }
// Test typeid in a template.

#include <typeinfo>

struct B { virtual void fn (); };
struct D : B { };

void abort ();

template<typename>
constexpr void
fn ()
{
  D d;
  if (&typeid (d) != &typeid (D))
   abort ();
}
constexpr bool b1 = (fn<int>(), true);

// Type-dependent.
template<typename T>
constexpr void
fn2 ()
{
  T t{};
  if (&typeid (t) != &typeid (T))
   abort ();
}
constexpr bool b2 = (fn2<int>(), true);
constexpr bool b3 = (fn2<B>(), true);
constexpr bool b4 = (fn2<D>(), true);
