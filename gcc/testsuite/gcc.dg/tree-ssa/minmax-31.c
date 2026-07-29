/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* These should produce 2x min for each function. */

__attribute__((noipa)) int
min_le (int a, int c)
{
  return (a < 5) ? (a < c ? a : c) : (5 < c ? 5 : c);
}

int
min_le_1 (int a, int c)
{
  if (a < 5)
    return (a < c ? a : c);
  return (5 < c ? 5 : c);
}

int
min_le_2 (int a, int c)
{
  int t = (a < c ? a : c);
  int t1 = (5 < c ? 5 : c);
  if (a < 5)
    return t;
  return t1;
}
/* { dg-final { scan-tree-dump-not "MAX_EXPR " "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 6 "optimized" } } */
