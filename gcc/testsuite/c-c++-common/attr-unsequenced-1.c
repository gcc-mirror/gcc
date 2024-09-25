/* Test gnu::unsequenced attribute: valid uses.  */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-std=gnu23" { target c } } */
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

[[gnu::unsequenced]] int f1 ();
[[gnu::unsequenced]] int f2 (), f3 (int);
int f4 (int, int *) [[gnu::unsequenced]];
int f5 (int) [[gnu::unsequenced]];
int f6 (int);
int (*fp1) (int) [[gnu::unsequenced]] = f6;
typedef int ft1 (int) [[gnu::unsequenced]];
typedef int ft2 (int);
#ifndef __cplusplus
extern __typeof (f6) [[gnu::unsequenced]] f7;
extern ft2 [[__gnu__::__unsequenced__]] f8;
#else
int f7 (int) [[gnu::unsequenced, gnu::unsequenced]];
int f8 (int) [[__gnu__::unsequenced, gnu::__unsequenced__]];
#endif
int f1 ();
int f9 (int);
int f9 (int) [[__gnu__::__unsequenced__]];
extern int x;

int
f10 (int x) [[gnu::unsequenced]]
{
  return x + 42;
}

int
f11 (int *x, long long y[1], int z) [[__gnu__::__unsequenced__]]
{
  x[0] = z;
  x[1] = z + 1;
  x[2] = z + 2;
  *y = z + 3;
  return z + 4 + f10 (-42);
}

int f12 () [[gnu::unsequenced]];
int f12 () [[gnu::reproducible]];
int f13 () [[gnu::reproducible]];
int f13 () [[gnu::unsequenced]];

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
