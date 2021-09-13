/* PR tree-optimization/95527 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump "a & 7\\) == 0" "original" } } */
/* { dg-final { scan-tree-dump "b & 63\\) != 0" "original" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "original" } } */
/* { dg-final { scan-tree-dump "g & 15\\) == 8" "original" } } */
/* { dg-final { scan-tree-dump "h & 255\\) != 128" "original" } } */

int
f1 (int a)
{
  return __builtin_ctz (a) >= 3;
}

int
f2 (int b)
{
  return __builtin_ctz (b) < 6;
}

int
f3 (int c)
{
  return __builtin_ctz (c) < 0;
}

int
f4 (int d)
{
  return __builtin_ctz (d) >= 0;
}

int
f5 (int e)
{
  return __builtin_ctz (e) >= __SIZEOF_INT__ * __CHAR_BIT__;
}

int
f6 (int f)
{
  return __builtin_ctz (f) < __SIZEOF_INT__ * __CHAR_BIT__;
}

int
f7 (int g)
{
  return __builtin_ctz (g) == 3;
}

int
f8 (int h)
{
  return __builtin_ctz (h) != 7;
}
