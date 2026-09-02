// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }
// { dg-options "" }

#include "../cpp2a/construct_at.h"

consteval bool
foo (int n)
{
  int a[n];
  if (!__builtin_is_within_lifetime (&a))
    return false;
  if (!__builtin_is_within_lifetime (&a[0]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n / 2]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n - 1]))
    return false;
  std::destroy_at (&a[0]);
  if (!__builtin_is_within_lifetime (&a))
    return false;
  if (__builtin_is_within_lifetime (&a[0]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n / 2]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n - 1]))
    return false;
  std::construct_at (&a[0]);
  std::destroy_at (&a[n / 2]);
  if (!__builtin_is_within_lifetime (&a))
    return false;
  if (!__builtin_is_within_lifetime (&a[0]))
    return false;
  if (__builtin_is_within_lifetime (&a[n / 2]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n - 1]))
    return false;
  std::construct_at (&a[n / 2]);
  std::destroy_at (&a[n - 1]);
  if (!__builtin_is_within_lifetime (&a))
    return false;
  if (!__builtin_is_within_lifetime (&a[0]))
    return false;
  if (!__builtin_is_within_lifetime (&a[n / 2]))
    return false;
  if (__builtin_is_within_lifetime (&a[n - 1]))
    return false;
  std::construct_at (&a[n - 1]);
  _Complex double b = 1.0;
  if (!__builtin_is_within_lifetime (&b))
    return false;
  if (!__builtin_is_within_lifetime (&__real__ b))
    return false;
  if (!__builtin_is_within_lifetime (&__imag__ b))
    return false;
  std::destroy_at (&__real__ b);
  if (!__builtin_is_within_lifetime (&b))
    return false;
  if (__builtin_is_within_lifetime (&__real__ b))
    return false;
  if (!__builtin_is_within_lifetime (&__imag__ b))
    return false;
  std::construct_at (&__real__ b);
  std::destroy_at (&__imag__ b);
  if (!__builtin_is_within_lifetime (&b))
    return false;
  if (!__builtin_is_within_lifetime (&__real__ b))
    return false;
  if (__builtin_is_within_lifetime (&__imag__ b))
    return false;
  std::construct_at (&__imag__ b);
  return true;
}

static_assert (foo (42));
