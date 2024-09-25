/* { dg-options "-O2 -fdump-tree-phiopt" } */

/* PR tree-optimization/116699
   Make sure the return PREDICT has no factor in deciding
   if we factor out the conversion. */

short f0(int a, int b, int c)
{
  int t1 = 4;
  if (c < t1)  return (c > -1 ? c : -1);
  return t1;
}


short f1(int a, int b, int c)
{
  int t1 = 4;
  short t = t1;
  if (c < t1)  t = (c > -1 ? c : -1);
  return t;
}

/* Both f1 and f0  should be optimized at phiopt1 to the same thing. */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 2  "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
