/* { dg-options "-O2 -fgraphite-identity -fno-tree-ch" } */
/* { dg-do run  } */
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

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

