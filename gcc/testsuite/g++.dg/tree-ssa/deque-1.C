// { dg-do compile } 
// { dg-options "-O1 -fdump-tree-optimized" }
// { dg-skip-if "required hosted libstdc++ for deque" { ! hostedlib } }

#include <deque>
void
test(std::deque<int> &q, int v)
{
  q.push_back (v);
}
// { dg-final { scan-tree-dump-not "throw_bad_alloc" "optimized" } }
