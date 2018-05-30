// PR c++/85873
// Test that these array temporaries are promoted to static variables as an
// optimization.

// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "= 42" "gimple" } }

#include <initializer_list>

int f()
{
  using AR = const int[];
  return AR{ 1,42,3,4,5,6,7,8,9,0 }[5];
}

int g()
{
  std::initializer_list<int> a = {1,42,3};
  return a.begin()[0];
}
