/* PR tree-optimization/20083 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */


int f(int i, int j, int l)
{
  int k = 0;
  if (i)
   k = 1;
  if (j)
   k = 1; 
  if (l)
    k = 1;
  return k;
}
int f1(int i, int j, int l)
{
  int k = 0;
  if (i)
   k = 1;
  else if (j)
   k = 1; 
  else if (l)
   k = 1;
  return k;
}
int f2(int i, int j, int l)
{
  return i||j||l;
}
int f3(int i, int j, int l)
{
  int t = i | j;
  _Bool t1 = l != 0;
  _Bool t2 = t ? 1 : t1;
  return t2;
}

int f4(int i, int j, int l)
{
  int t = i | j;
  _Bool t1 = l != 0;
  _Bool t2;
  if (t)
   t2 = 1;
  else
   t2 = t1;
  return t2;
}

/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* 2 ior for each function. */
/* { dg-final { scan-tree-dump-times "bit_ior_expr," 10 "optimized"  } } */
/* { dg-final { scan-tree-dump-times "ne_expr," 5 "optimized"  } } */
