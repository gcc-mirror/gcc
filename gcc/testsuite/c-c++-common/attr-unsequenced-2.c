/* Test unsequenced attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " f1 \\\(\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f2 \\\(\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f12 \\\(\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f13 \\\(\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f3 \\\(42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f5 \\\(42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f7 \\\(42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f8 \\\(42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f9 \\\(42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f3 \\\(-42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f5 \\\(-42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f7 \\\(-42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f8 \\\(-42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " f9 \\\(-42\\\);" 1 "optimized" } } */
/* { dg-final { scan-tree-dump " f3 \\\(52\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times " fp1\.\[0-9]*_\[0-9]* \\\(14\\\);" 1 "optimized" } } */

__attribute__((unsequenced)) int f1 (void);
__attribute__((unsequenced)) int f2 (void), f3 (int);
int f4 (int, int *) __attribute__((unsequenced));
int f5 (int) __attribute__((unsequenced));
int f6 (int);
int (*fp1) (int) __attribute__((unsequenced)) = f6;
typedef int ft1 (int) __attribute__((unsequenced));
typedef int ft2 (int);
extern __typeof (f6) __attribute__((unsequenced)) f7;
extern ft2 __attribute__((__unsequenced__)) f8;
int f1 (void);
int f9 (int);
int f9 (int) __attribute__((__unsequenced__));
extern int x;

__attribute__((unsequenced)) int
f10 (int x)
{
  return x + 42;
}

__attribute__((__unsequenced__)) int
f11 (int *x, long long y[1], int z)
{
  x[0] = z;
  x[1] = z + 1;
  x[2] = z + 2;
  *y = z + 3;
  return z + 4 + f10 (-42);
}

int f12 (void) __attribute__((unsequenced));
int f12 (void) __attribute__((reproducible));
int f13 (void) __attribute__((reproducible));
int f13 (void) __attribute__((unsequenced));

int
g (void)
{
  int a = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42) + f12 () + f13 ();
  int b = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42) + f12 () + f13 ();
  int c = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int d = f3 (-42) + f5 (-42) + f7 (-42) + f8 (-42) + f9 (-42);
  int e = fp1 (14) + fp1 (14);
  x++;
  int f = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42) + f12 () + f13 ();
  int g = f1 () + f2 () + f3 (42) + f5 (42) + f7 (42) + f8 (42) + f9 (42) + f12 () + f13 ();
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
