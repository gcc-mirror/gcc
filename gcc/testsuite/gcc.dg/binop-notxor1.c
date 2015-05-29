/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (_Bool a, _Bool b)
{
  return (a ^ !a) | (!b ^ b);
}

/* { dg-final { scan-tree-dump-times "return 1" 1 "optimized" } } */
