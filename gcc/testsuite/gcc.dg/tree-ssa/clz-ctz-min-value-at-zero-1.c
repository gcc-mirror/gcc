/* MIN of two .CLZ or two .CTZ calls becomes one call on the IOR of the
   arguments only if the value at zero is at least the precision.  */
/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target clzsi } */
/* { dg-require-effective-target ctzsi } */
/* { dg-options "-O2 -fgimple -fdump-tree-optimized" } */

/* For X = 0 and Y = 5 the MIN is -1, but .CLZ (X | Y, -1) is 29.  */

int __GIMPLE (ssa)
clzm1 (unsigned int x, unsigned int y)
{
  int _1;
  int _2;
  int _3;

  __BB(2):
  _1 = .CLZ (x_4(D), -1);
  _2 = .CLZ (y_5(D), -1);
  _3 = __MIN (_1, _2);
  return _3;
}

int __GIMPLE (ssa)
ctzm1 (unsigned int x, unsigned int y)
{
  int _1;
  int _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), -1);
  _2 = .CTZ (y_5(D), -1);
  _3 = __MIN (_1, _2);
  return _3;
}

/* 32 is not smaller than any count of a nonzero value.  */

int __GIMPLE (ssa)
clz32 (unsigned int x, unsigned int y)
{
  int _1;
  int _2;
  int _3;

  __BB(2):
  _1 = .CLZ (x_4(D), 32);
  _2 = .CLZ (y_5(D), 32);
  _3 = __MIN (_1, _2);
  return _3;
}

int __GIMPLE (ssa)
ctz32 (unsigned int x, unsigned int y)
{
  int _1;
  int _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), 32);
  _2 = .CTZ (y_5(D), 32);
  _3 = __MIN (_1, _2);
  return _3;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\| " 2 "optimized" } } */
