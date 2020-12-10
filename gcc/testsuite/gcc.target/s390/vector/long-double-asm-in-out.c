/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

int
main (void)
{
  long double res, x = 0x1.0000000000001p+0L,
		   exp = 1.00000000000000011102230246251564788e+0L;
  asm("sqxbr\t%0,%1" : "=f"(res) : "f"(x));
  assert (res == exp);
}
