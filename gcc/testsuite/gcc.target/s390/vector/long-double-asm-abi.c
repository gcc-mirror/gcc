/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
xsqrt (long double x)
{
  long double res;
  asm("sqxbr\t%0,%1" : "=f"(res) : "f"(x));
  return res;
}

/* Check that the generated code is very small and straightforward.  In
   particular, there must be no unnecessary copying and no stack frame.  */
/* { dg-final { scan-assembler {\n\tld\t[^\n]*;ld\t[^\n]*\n(#[^\n]*\n)*\tsqxbr\t.*\n(#[^\n]*\n)*\tstd\t[^\n]*\n\tstd\t[^\n]*\n\tbr\t%r14\n} } } */

int
main (void)
{
  long double res, x = 0x1.0000000000001p+0L,
		   exp = 1.00000000000000011102230246251564788e+0L;
  res = xsqrt (x);
  assert (res == exp);
}
