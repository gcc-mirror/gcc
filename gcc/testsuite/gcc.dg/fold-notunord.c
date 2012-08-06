/* { dg-do compile } */
/* { dg-options "-O -ftrapping-math -fdump-tree-optimized" } */

int f (double d)
{
  return !__builtin_isnan (d);
}

/* { dg-final { scan-tree-dump " ord " "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
