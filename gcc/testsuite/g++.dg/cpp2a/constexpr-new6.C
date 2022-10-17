// P0784R7
// { dg-do compile { target c++20 } }

#include "construct_at.h"

constexpr bool
foo ()
{
  std::allocator<int> a;
  auto p = a.allocate (2);
  std::construct_at (p, 1);
  std::construct_at (p + 1, 2);
  if (p[0] != 1 || p[1] != 2)
    throw 1;
  std::destroy_at (p);
  std::destroy_at (p + 1);
  a.deallocate (p, 2);
  return true;
}

static_assert (foo ());
