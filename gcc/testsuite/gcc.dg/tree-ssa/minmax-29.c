/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* These should not produce max, only 3 min and there should be an if left. */

__attribute__((noipa)) int
min_le (int a, int c)
{
  return (a <= 6) ? (a < c ? a : c) : (5 < c ? 5 : c);
}

int
min_le_1 (int a, int c)
{
  if (a <= 6)
    return (a < c ? a : c);
  return (5 < c ? 5 : c);
}

int
min_le_2 (int a, int c)
{
  int t = (a < c ? a : c);
  int t1 = (5 < c ? 5 : c);
  if (a <= 6)
    return t;
  return t1;
}
/* { dg-final { scan-tree-dump-not "MAX_EXPR " "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "if " 3 "optimized" } } */
