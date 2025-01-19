// { dg-do compile }
// { dg-options "-O3 -fdump-tree-optimized" }
#include <deque>
std::deque<int *>
test2(std::deque<int *> &q)
{
  return q;
}
// rethrow is OK, but throw is not.
// { dg-final { scan-tree-dump-not {[^e]throw} "optimized" } }
