/* { dg-do run } */
/* { dg-options "-save-temps" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_condition } */
#include <stdlib.h>

int fn1 (int) __attribute__ ((noinline));

int a[128];
int fn1(int d) {
  int b, c = 1;
  for (b = 0; b < 128; b++)
    if (a[b])
      c = 0;
  return c;
}

int
main (void)
{
  int i;
  for (i = 0; i < 128; i++)
    a[i] = 0;
  if (fn1(10) != 1)
    abort ();
  a[3] = 2;
  a[24] = 1;
  if (fn1(10) != 0)
    abort ();
  return 0;
}
/* { dg-final { scan-assembler-not "\[ \t\]not\[ \t\]" } } */
