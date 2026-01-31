// PR c++/123825
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::has_identifier.

#include <meta>

void fun (int);
constexpr std::meta::info r = parameters_of (^^fun)[0];
static_assert (!has_identifier (r));

void fun (int x);
static_assert (has_identifier (r));

void fun (int x);
static_assert (has_identifier (r));

void
poison ()
{
  void fun (int y);
}
static_assert (!has_identifier (r));
