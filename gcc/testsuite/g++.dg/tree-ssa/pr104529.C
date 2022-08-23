// PR middle-end/104529
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-not "MEM\[^\n\r]*MEM" "optimized" } }

#include <cstddef>
#include <vector>

struct S {
  unsigned int a;
  std::vector<unsigned char> b;
  std::vector<unsigned char> c;
};

std::size_t
foo ()
{
  S test[] = { { 48, { 255, 0, 0, 0, 0, 0 } } };
  return sizeof (test);
}
