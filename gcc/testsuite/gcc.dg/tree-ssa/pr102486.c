/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f (unsigned y)
{
  return __builtin_popcount (y & -y);
}

int f2 (int y)
{
  return __builtin_popcount (y & -y);
}

/* { dg-final { scan-tree-dump-times "popcount" 0 "optimized" } } */
