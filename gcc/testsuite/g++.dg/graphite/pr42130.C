/* { dg-options "-O2 -fgraphite-identity -fno-tree-ch" } */
#include <vector>

using std::vector;

vector<unsigned> & __attribute__((noinline, noclone)) foo(unsigned n)
{
  vector<unsigned> *vv = new vector<unsigned>(n, 0u);
  return *vv;
}

int main()
{
  foo(0);
  return 0;
}

/* { dg-do run  } */

