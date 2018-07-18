// { dg-do compile { target c++14 } }
// { dg-options "-O2 -fdump-tree-optimized" }

#include <algorithm>

static int g()
{
      return 1234;
}

int f2()
{
      return std::min({1, g(), 4});
}

// { dg-final { scan-tree-dump "return 1;" "optimized" } }
