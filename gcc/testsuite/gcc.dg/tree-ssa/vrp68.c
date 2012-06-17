/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

#include "vrp.h"

void test1 (int i, int j, int b)
{
  RANGE(i, 2, 6);
  ANTI_RANGE(j, 1, 7);
  MERGE(b, i, j);
  CHECK_ANTI_RANGE(i, 7, 7);
  CHECK_ANTI_RANGE(i, 1, 1);
  /* If we swap the anti-range tests the ~[6, 6] test is never eliminated.  */
}
int main() { }

/* While subsequent VRP/DOM passes manage to even recognize the ~[6, 6]
   test as redundant a single VRP run will arbitrarily choose ~[0, 0] when
   merging [1, 5] with ~[0, 6] so the first VRP pass can only eliminate
   the ~[0, 0] check as redundant.  */

/* { dg-final { scan-tree-dump-times "link_error" 0 "vrp1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "link_error" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
