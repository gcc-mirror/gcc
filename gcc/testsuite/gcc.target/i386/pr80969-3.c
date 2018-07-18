/* { dg-do run { target { { ! x32 } && avx512f_runtime } } } */
/* { dg-do compile { target { { ! x32 } && { ! avx512f_runtime } } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f" } */
/* { dg-require-effective-target avx512f } */

/* Test with alloca (and DRAP).  */

#include <alloca.h>

int a[56];
volatile int b = -12345;
volatile const int d = 42;

void foo (int *x, int y, int z)
{
}

void (*volatile const foo_noinfo)(int *, int, int) = foo;

int main (int argc, char *argv[]) {
  int c;
  int *e = alloca (d);
  foo_noinfo (e, d, 0);
  for (; b; b++) {
    c = b;
    if (b & 1)
      c = 2;
    foo_noinfo (e, d, c);
    a[-(b % 56)] = c;
  }
  return 0;
}
