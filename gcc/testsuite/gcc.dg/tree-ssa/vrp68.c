/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

#include "vrp.h"

void test1 (int i, int j, int b)
{
  RANGE(i, 2, 6);
  ANTI_RANGE(j, 1, 7);
  MERGE(b, i, j);
  CHECK_ANTI_RANGE(i, 1, 1);
}
int main() { }

/* VRP will arbitrarily choose ~[1, 1] when merging [2, 6] with ~[1, 7].  */

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
