/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

_Bool f1(unsigned x, unsigned y)
{
  unsigned t1 = x / y;
  _Bool t2 = (t1 != 0);
  return t2;
}

_Bool f2(unsigned x, unsigned y)
{
  unsigned t1 = x / y;
  _Bool t2 = (t1 == 0);
  return t2;
}

/* { dg-final { scan-tree-dump-not "trunc_div_expr" "optimized" } } */
