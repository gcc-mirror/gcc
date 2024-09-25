// PR c++/59329
// { dg-do compile { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

inline constexpr int exampleFunction(int min, int max)
{
  assert(min <= max);
  return min + max;
}
