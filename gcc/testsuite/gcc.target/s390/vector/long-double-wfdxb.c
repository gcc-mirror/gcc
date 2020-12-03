/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static long double
wfdxb (long double x, long double y, long double z)
{
  return (x / y) / z;
}

/* { dg-final { scan-assembler-times {\n\twfdxb\t} 2 } } */

int
main (void)
{
  assert (wfdxb (2.22L, 1.11L, 2.L) == 1.L);
}
