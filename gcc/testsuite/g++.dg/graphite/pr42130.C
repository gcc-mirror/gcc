/* { dg-options "-O2 -fno-tree-ch" } */
#include <vector>

using std::vector;

vector<unsigned> & __attribute__((noinline)) foo(unsigned n, unsigned k)
{
  vector<unsigned> *vv = new vector<unsigned>(n, 0u);
  return *vv;
}


int main()
{
  foo(0, 1);
}
/* { dg-do run  } */

