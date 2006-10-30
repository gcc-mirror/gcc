/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return ~a == ~b;
}

int test2(int c, int d)
{
  return -c == -d;
}

int test3(int e)
{
  return -e == 5;
}

int test4(int f)
{
  return ~f == 5;
}

int test5(int g, int h)
{
  return ~g < ~h;
}

int test6(int i, int j)
{
  return ~i >= ~j;
}

int test7(int k)
{
  return ~k < 3;
}

int test8(int l)
{
  return ~l >= 2;
}

/* { dg-final { scan-tree-dump-times "b == a" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c == d" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e == -5" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "f == -6" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "h < g" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "j >= i" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "k >= -3" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "l < -2" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */

