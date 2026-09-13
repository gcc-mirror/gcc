/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32 } */

/* The branchless min and max that x264's x264_median writes.  The existing
   rule turns x & (x >> 31) into MIN <x, 0>; these have to finish the job.  */

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

int
swar_min_not (int a, int b)
{
  int t = a - b;
  return a - (t & ~(t >> 31));
}

int
swar_max_not (int a, int b)
{
  int t = a - b;
  return b + (t & ~(t >> 31));
}

/* Both uses of the one difference have to fold.  */
int
swar_median (int a, int b, int c)
{
  int t = (a - b) & ((a - b) >> 31);
  a -= t;
  b += t;
  b -= (b - c) & ((b - c) >> 31);
  b += (a - b) & ((a - b) >> 31);
  return b;
}

/* The existing rule leaves MIN <x, 0> and MAX <x, 0> behind, so the counts
   alone do not say whether this patch fired; what says it is that none of the
   clamps against zero survive.  The four simple functions fold in forwprop1,
   the median needs one more pass because its uses are chained.  */
/* { dg-final { scan-tree-dump-not "MIN_EXPR <\[^,\]*, 0>" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR <\[^,\]*, 0>" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR <\[^,\]*, 0>" "optimized" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR <\[^,\]*, 0>" "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 4 "optimized" } } */
