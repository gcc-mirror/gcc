/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

#include <new>

int *r;

void test()
{
  r = new(std::nothrow) int;
  if (r)
    __builtin_memset (r, 0, sizeof(int));
}

/* { dg-final { scan-tree-dump "malloc returned non-NULL heuristics of edge\[^:\]*: 99.96%" "profile_estimate"} } */
