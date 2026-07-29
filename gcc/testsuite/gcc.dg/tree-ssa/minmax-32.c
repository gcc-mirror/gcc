/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* These should produce 2x max for each function. */
int
max_ge (int a, int c)
{
  return (a > 4) ? (a > c ? a : c) : (5 > c ? 5 : c);
}

int
max_ge_1 (int a, int c)
{
  int t = (a > c ? a : c);
  int t1 = (5 > c ? 5 : c);
  return (a > 4) ? t : t1;
}

int
max_ge_2 (int a, int c)
{
  if (a > 4)
    return (a > c ? a : c);
  return (5 > c ? 5 : c);
}


/* { dg-final { scan-tree-dump-times "MAX_EXPR " 6 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR " "optimized" } } */
