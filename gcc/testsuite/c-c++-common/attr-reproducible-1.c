/* Test gnu::reproducible attribute: valid uses.  */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-std=gnu23" { target c } } */
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

int f1 () [[gnu::reproducible]];
int f2 () [[gnu::reproducible]], f3 (int) [[__gnu__::__reproducible__]];
int f4 (int, int *) [[gnu::reproducible]];
int f5 (int) [[gnu::reproducible]];
int f6 (int);
int (*fp1) (int) [[gnu::reproducible]] = f6;
typedef int ft1 (int) [[gnu::reproducible]];
typedef int ft2 (int);
#ifndef __cplusplus
extern __typeof (f6) [[gnu::reproducible]] f7;
extern ft2 [[__gnu__::__reproducible__]] f8;
#else
int f7 (int) [[gnu::reproducible, gnu::reproducible]];
int f8 (int) [[__gnu__::reproducible, gnu::__reproducible__]];
#endif
int f1 ();
int f9 (int);
int f9 (int) [[__gnu__::__reproducible__]];
extern int x;

int
f10 (int w) [[gnu::reproducible]]
{
  return w + 42 + x;
}

int
f11 (int *w, long long y[1], int z) [[__gnu__::__reproducible__]]
{
  w[0] = z + x;
  w[1] = z + x + 1;
  w[2] = z + x + 2;
  *y = z + x + 3;
  return z + 4 + f10 (-42);
}

int
g ()
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
h ()
{
  f3 (52);
  f3 (52);
  f3 (52);
  return 0;
}
