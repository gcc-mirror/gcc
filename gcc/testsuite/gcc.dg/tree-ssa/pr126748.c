/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

unsigned f(unsigned a, unsigned b)
{
    if (b >= a) __builtin_unreachable();
    return b / a;
}

int fs(int a, int b)
{
  a = __builtin_abs(a);
  b = __builtin_abs(b);
  if (b >= a) __builtin_unreachable();
  return b / a;
}

/* This can't be simplified.  */
int fs2(int a, int b)
{
  if (b >= a) __builtin_unreachable();
  return b / a;
}

/* { dg-final { scan-tree-dump-times "return 0;" 2 "evrp" } } */
/* { dg-final { scan-tree-dump-times " / " 1 "evrp" } } */
