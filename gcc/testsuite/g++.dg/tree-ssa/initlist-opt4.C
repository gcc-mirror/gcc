// PR c++/110070
// { dg-additional-options -fdump-tree-gimple }
// { dg-do compile { target c++11 } }

// { dg-final { scan-tree-dump {static const int [^\n]*\[4\] = } "gimple" } }

#include <initializer_list>
extern void ext(int);
void foo()
{
  for (int i: {1,2,4,6})
    ext(i);
}
