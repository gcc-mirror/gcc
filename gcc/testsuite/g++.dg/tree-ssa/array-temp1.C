// PR c++/85873
// Test that the array temporary is promoted to a static variable as an
// optimization.

// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not "= 42" "gimple" } }

#include <initializer_list>

int g()
{
  std::initializer_list<int> a = {1,42,3};
  return a.begin()[0];
}
