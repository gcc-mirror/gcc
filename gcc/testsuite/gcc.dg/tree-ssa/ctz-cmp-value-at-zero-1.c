/* .CTZ (X, V) >= C becomes (X & ((1 << C) - 1)) == 0 only if V >= C,
   because the bit test is true for X = 0.  */
/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target ctzsi } */
/* { dg-options "-O2 -fgimple -fdump-tree-optimized" } */

/* .CTZ (0, -1) is -1, which is smaller than 3.  */

int __GIMPLE (ssa)
gem1 (unsigned int x)
{
  int _1;
  _Bool _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), -1);
  _2 = _1 >= 3;
  _3 = (int) _2;
  return _3;
}

int __GIMPLE (ssa)
ltm1 (unsigned int x)
{
  int _1;
  _Bool _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), -1);
  _2 = _1 < 3;
  _3 = (int) _2;
  return _3;
}

/* .CTZ (0, 32) is 32, which is not smaller than 3.  */

int __GIMPLE (ssa)
ge32 (unsigned int x)
{
  int _1;
  _Bool _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), 32);
  _2 = _1 >= 3;
  _3 = (int) _2;
  return _3;
}

int __GIMPLE (ssa)
lt32 (unsigned int x)
{
  int _1;
  _Bool _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), 32);
  _2 = _1 < 3;
  _3 = (int) _2;
  return _3;
}

/* { dg-final { scan-tree-dump-times "\\.CTZ" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 7;" 2 "optimized" } } */
