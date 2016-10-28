/* { dg-require-effective-target divmod_simode } */
/* { dg-options "-O2 -fdump-tree-widening_mul-details" } */
/* div and mod are not in same bb and
   bb's containing div and mod don't dominate each other.  */

int f(int x, int y)
{
  int q = 0;
  int r = 0;
  extern int cond;

  if (cond)
    q = x / y;

  r = x % y;
  return q + r;
}

/* { dg-final { scan-tree-dump-times "DIVMOD" 0 "widening_mul" } } */
