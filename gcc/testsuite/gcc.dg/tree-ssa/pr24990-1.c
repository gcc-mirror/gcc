/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(int x)
{
  return ((~x) != 0);
}
int f1(int x)
{
  return ((~x) == 0);
}

/* There should be no != 0 which is produced by the front-end as
   ~x != 0 is the same as x != -1 (or ~0).   Likewise for ==. */
/* { dg-final { scan-tree-dump-times "!= 0" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "!= -1" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "== 0" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "== -1" 1 "optimized"} } */

/* { dg-final { cleanup-tree-dump "optimized" } } */

