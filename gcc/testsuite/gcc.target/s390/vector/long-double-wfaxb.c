/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

__attribute__ ((noipa)) static long double
wfaxb (long double x, long double y, long double z)
{
  return x + y + z;
}

/* { dg-final { scan-assembler-times {\n\twfaxb\t} 2 } } */

int
main (void)
{
  assert (wfaxb (1.11L, 2.22L, 3.33L) == 6.66L);
}
