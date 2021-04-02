/* PR tree-optimization/94802 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " = __builtin_clz " "optimized" } } */

__attribute__((noipa)) int
f1 (int a, int b)
{
  return __builtin_clz (a - b) != 0;
}

__attribute__((noipa)) int
f2 (int x)
{
  return __builtin_clz (x) == 0;
}

__attribute__((noipa)) int
f3 (int x)
{
  return __builtin_clz (x) != 0;
}

__attribute__((noipa)) int
f4 (int a, int b)
{
  return __builtin_clz (a - b) == sizeof (int) * __CHAR_BIT__ - 1;
}

__attribute__((noipa)) int
f5 (int x)
{
  return __builtin_clz (x) == sizeof (int) * __CHAR_BIT__ - 1;
}

__attribute__((noipa)) int
f6 (int x)
{
  return __builtin_clz (x) != sizeof (int) * __CHAR_BIT__ - 1;
}

int
main ()
{
  if (f1 (5, 7) != 0
      || f1 (7, 5) != 1
      || f2 (1) != 0
      || f2 (137) != 0
      || f2 (-1) != 1
      || f2 (-137) != 1
      || f3 (1) != 1
      || f3 (137) != 1
      || f3 (-1) != 0
      || f3 (-137) != 0
      || f4 (5, 4) != 1
      || f4 (6, 4) != 0
      || f4 (4, 5) != 0
      || f5 (1) != 1
      || f5 (17) != 0
      || f5 (-1) != 0
      || f5 (-17) != 0
      || f6 (1) != 0
      || f6 (17) != 1
      || f6 (-1) != 1
      || f6 (-17) != 1)
    __builtin_abort ();
  return 0;
}
