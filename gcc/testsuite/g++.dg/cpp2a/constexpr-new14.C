// PR c++/97195
// { dg-do compile { target c++20 } }

#include "construct_at.h"

constexpr bool
foo ()
{
  int a = 5;
  int *p = std::construct_at (&a, -1);
  if (p[0] != -1)
    throw 1;
  return true;
}
constexpr bool b = foo ();
