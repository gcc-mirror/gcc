/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

/* With a wrapping difference the identity fails: for a = INT_MIN, b = 1 the
   difference wraps positive and the branchless form returns b.  The inner
   MIN <a - b, 0> is still formed, but neither function may become a plain
   MIN or MAX of a and b.  */

int
swar_min (int a, int b)
{
  int t = a - b;
  return b + (t & (t >> 31));
}

int
swar_max (int a, int b)
{
  int t = a - b;
  return a - (t & (t >> 31));
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR <\[^,\]*, 0>" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR" "optimized" } } */
