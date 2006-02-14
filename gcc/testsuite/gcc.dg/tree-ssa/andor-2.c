/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return (a & b) | b;
}

int test2(int c, int d)
{
  return (c & d) | c;
}

int test3(int e, int f)
{
  return e | (e & f);
}

int test4(int g, int h)
{
  return g | (h & g);
}

int test5(int i, int j)
{
  return (i | j) & j;
}

int test6(int k, int l)
{
  return (k | l) & k;
}

int test7(int m, int n)
{
  return m & (m | n);
}

int test8(int o, int p)
{
  return o & (p | o);
}

/* { dg-final { scan-tree-dump-times "return b;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return c;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return e;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return g;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return j;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return k;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return m;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return o;" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
