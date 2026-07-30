/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned int u32;

/* While the MIN stays live the saturating subtract would be computed beside
   it rather than instead of it, so the rules do not fire.  */

u32
min_live (u32 a, u32 b, u32 *o)
{
  u32 m = a < b ? a : b;
  *o = m;
  return a - m;
}

/* { dg-final { scan-tree-dump-not "\\.SAT_SUB " "optimized" } } */
