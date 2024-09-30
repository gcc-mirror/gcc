/* Test reproducible attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " f1 \\\(\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f2 \\\(\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f3 \\\(42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f5 \\\(42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f7 \\\(42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f8 \\\(42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f9 \\\(42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f3 \\\(-42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f5 \\\(-42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f7 \\\(-42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f8 \\\(-42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f9 \\\(-42\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump " f3 \\\(52\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times " fp1\.\[0-9]*_\[0-9]* \\\(14\\\);" 2 "optimized" } } */

__attribute__((reproducible)) int f1 (void);
__attribute__((__reproducible__)) int f2 (void), f3 (int);
int f4 (int, int *) __attribute__((reproducible));
int f5 (int) __attribute__((reproducible));
int f6 (int);
int (*fp1) (int) __attribute__((reproducible)) = f6;
typedef int ft1 (int) __attribute__((reproducible));
typedef int ft2 (int);
extern __typeof (f6) __attribute__((reproducible)) f7;
extern ft2 __attribute__((__reproducible__)) f8;
int f1 (void);
int f9 (int);
int f9 (int) __attribute__((__reproducible__));
extern int x;

__attribute__((reproducible)) int
f10 (int w)
{
  return w + 42 + x;
}

__attribute__((reproducible)) int
f11 (int *w, long long y[1], int z)
{
  w[0] = z + x;
  w[1] = z + x + 1;
  w[2] = z + x + 2;
  *y = z + x + 3;
  return z + 4 + f10 (-42);
}

int
g (void)
{
  int a = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42);
  int b = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42);
  int c = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int d = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int e = fp1 (14) + fp1 (14);
  x++;
  int f = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42);
  int g = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42);
  int h = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int i = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int j = fp1 (14) + fp1 (14);
  return a + b + c + d + e + f + g + h + i + j;
}

int
h (void)
{
  f3 (52);
  f3 (52);
  f3 (52);
  return 0;
}
