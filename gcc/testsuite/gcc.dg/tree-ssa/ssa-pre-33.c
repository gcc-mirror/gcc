/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

_Bool f1(unsigned x, unsigned y, unsigned *res, int flag, _Bool *t)
{
  if (flag)
    *t = __builtin_add_overflow(x, y, res);
  unsigned res1;
  _Bool t1 = __builtin_add_overflow(x, y, &res1);
  *res -= res1;
  return *t==t1;
}

/* We should hoist the .ADD_OVERFLOW to before the check.  */
/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 1 "pre" } } */
