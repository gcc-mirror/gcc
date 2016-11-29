/* { dg-require-effective-target divmod_simode } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */

int f(int x, int y)
{
  int q = 0, r1 = 0, r2 = 0;
  extern int cond;

  if (cond)
    q = x / y;
  else
    {
      r1 = x % y;
      return q + r1;
    }

  r2 = x % y;
  return q + r2;
}

/* { dg-final { scan-tree-dump-times "DIVMOD" 1 "widening_mul" } } */
