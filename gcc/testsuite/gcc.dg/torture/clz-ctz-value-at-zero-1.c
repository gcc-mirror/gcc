/* Two-argument .CLZ and .CTZ give their second argument for zero, also
   when the instruction of the target gives another value.  */
/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target clzsi } */
/* { dg-require-effective-target ctzsi } */
/* { dg-additional-options "-fgimple" } */

int __GIMPLE (ssa) __attribute__((noipa))
clz5 (unsigned int x)
{
  int _1;

  __BB(2):
  _1 = .CLZ (x_2(D), 5);
  return _1;
}

int __GIMPLE (ssa) __attribute__((noipa))
ctzm7 (unsigned int x)
{
  int _1;

  __BB(2):
  _1 = .CTZ (x_2(D), -7);
  return _1;
}

int __GIMPLE (ssa) __attribute__((noipa))
ctz5eq32 (unsigned int x)
{
  int _1;
  _Bool _3;
  int _4;

  __BB(2):
  _1 = .CTZ (x_2(D), 5);
  _3 = _1 == 32;
  _4 = (int) _3;
  return _4;
}

int __GIMPLE (ssa) __attribute__((noipa))
clzconst (void)
{
  int _1;

  __BB(2):
  _1 = .CLZ (0u, 5);
  return _1;
}

int __GIMPLE (ssa) __attribute__((noipa))
clzand (unsigned int a, unsigned int b)
{
  unsigned int _3;
  int _4;

  __BB(2):
  _3 = a_1(D) & b_2(D);
  _4 = .CLZ (_3, 5);
  return _4;
}

int __GIMPLE (ssa) __attribute__((noipa))
ctztab (unsigned int x)
{
  int _1;
  int _2;

  __BB(2):
  _1 = .CTZ (x_3(D), 32);
  _2 = _1 & 31;
  return _2;
}

int __GIMPLE (ssa) __attribute__((noipa))
clz32 (unsigned int x)
{
  int _1;

  __BB(2):
  _1 = .CLZ (x_2(D), 32);
  return _1;
}

/* The MIN of .CLZ (0, -1) and .CLZ (5, -1) is -1.  */

int __GIMPLE (ssa) __attribute__((noipa))
minclzm1 (unsigned int x, unsigned int y)
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

/* .CTZ (0, -1) is -1, which is smaller than 3 and not equal to 3.  */

int __GIMPLE (ssa) __attribute__((noipa))
ctzgem1 (unsigned int x)
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

int __GIMPLE (ssa) __attribute__((noipa))
ctzeqm1 (unsigned int x)
{
  int _1;
  _Bool _2;
  int _3;

  __BB(2):
  _1 = .CTZ (x_4(D), -1);
  _2 = _1 == 3;
  _3 = (int) _2;
  return _3;
}

int
main (void)
{
  if (clz5 (0) != 5 || clz5 (1) != 31 || clz5 (0x80000000u) != 0)
    __builtin_abort ();
  if (ctzm7 (0) != -7 || ctzm7 (8) != 3)
    __builtin_abort ();
  if (ctz5eq32 (0) != 0 || ctz5eq32 (1) != 0)
    __builtin_abort ();
  if (clzconst () != 5)
    __builtin_abort ();
  if (clzand (0xf0, 0x0f) != 5 || clzand (3, 1) != 31)
    __builtin_abort ();
  if (ctztab (0) != 0 || ctztab (8) != 3)
    __builtin_abort ();
  if (clz32 (0) != 32 || clz32 (1) != 31)
    __builtin_abort ();
  if (minclzm1 (0, 5) != -1 || minclzm1 (4, 5) != 29)
    __builtin_abort ();
  if (ctzgem1 (0) != 0 || ctzgem1 (8) != 1 || ctzgem1 (4) != 0)
    __builtin_abort ();
  if (ctzeqm1 (0) != 0 || ctzeqm1 (8) != 1)
    __builtin_abort ();
  return 0;
}
