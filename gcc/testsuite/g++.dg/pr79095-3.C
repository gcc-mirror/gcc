/* { dg-do compile } */
/* { dg-options "-Wall -O3" } */
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <vector>

void foo(std::vector<unsigned int> &v);

void vtest()
{
  std::vector<unsigned int> v;
  foo (v);
  if (v.size() > 0)
  {
    v.resize (v.size()-1);
  }
}

