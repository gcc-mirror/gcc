/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f(int *a, int *b)
{
  __PTRDIFF_TYPE__ t = a - b;
  if (t < 0)
    return a < b;
  return 0;
}

int g();

int f1(int *a, int *b)
{
  __PTRDIFF_TYPE__ t = a - b;
  if (t < 0)
   {
     if (a < b)
       return g();
   }
  return 0;
}

/* { dg-final { scan-tree-dump-times " < " 2 "optimized" } } */
/* Only 1 if left for f1 */
/* { dg-final { scan-tree-dump-times "if " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " != " "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
