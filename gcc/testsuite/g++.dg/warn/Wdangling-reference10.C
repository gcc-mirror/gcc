// PR c++/107532
// { dg-do compile { target c++20 } }
// { dg-options "-Wdangling-reference" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <span>
#include <vector>

void f(const std::vector<int>& v)
{
  const int& r = std::span<const int>(v)[0]; // { dg-bogus "dangling reference" }
  (void) r;
}
