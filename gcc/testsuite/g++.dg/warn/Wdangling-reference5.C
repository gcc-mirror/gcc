// PR c++/107488
// { dg-do compile }
// { dg-options "-Wdangling-reference" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

int
do_sum (std::vector<int>& v)
{
  int sum = 0;

  std::vector<int>::const_iterator it = v.begin();
  while (it != v.end())
    {
      // R refers to one of the int elements of V, not to a temporary
      // object, so no dangling reference here.
      const int &r = *it++; // { dg-bogus "dangling reference" }
      sum += r;
    }

  return sum;
}
