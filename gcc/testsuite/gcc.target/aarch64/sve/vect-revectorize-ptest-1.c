/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -fno-trapping-math -march=armv8-a+sve" } */

/* PTEST must ignore the predicate bits between 64-bit elements.  */

const double a[] = { 0, 1, 2, 3, 4, 5 };

__attribute__((noipa))
int
f (double x)
{
  int i = 0;
#pragma GCC unroll 0
  while (i < 6 && a[i] <= x)
    ++i;
  return i;
}

int
main (void)
{
  return f (2.0) != 3;
}
