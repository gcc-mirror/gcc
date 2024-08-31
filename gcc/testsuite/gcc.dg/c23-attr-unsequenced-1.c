/* Test C23 unsequenced attribute: valid uses.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -O2 -fdump-tree-optimized" } */
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

int f1 () [[unsequenced]];
int f2 () [[unsequenced]], f3 (int) [[__unsequenced__]];
int f4 (int, int *restrict) [[unsequenced]];
int f5 (int) [[unsequenced]];
int f6 (int);
int (*fp1) (int) [[unsequenced]] = f6;
typedef int ft1 (int) [[unsequenced]];
typedef int ft2 (int);
extern typeof (f6) [[unsequenced]] f7;
extern ft2 [[__unsequenced__]] f8;
int f1 ();
int f9 (int);
int f9 (int) [[__unsequenced__]];
extern int x;

int
f10 (int x) [[unsequenced]]
{
  return x + 42;
}

int
f11 (int *restrict x, long long y[restrict static 1], int z) [[__unsequenced__]]
{
  x[0] = z;
  x[1] = z + 1;
  x[2] = z + 2;
  *y = z + 3;
  return z + 4 + f10 (-42);
}

int f12 () [[unsequenced]];
int f12 () [[reproducible]];
int f13 () [[reproducible]];
int f13 () [[unsequenced]];

int
g ()
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
h ()
{
  f3 (52);
  f3 (52);
  f3 (52);
  return 0;
}
