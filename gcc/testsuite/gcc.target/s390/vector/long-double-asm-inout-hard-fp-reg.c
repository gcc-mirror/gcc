/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
sqxbr (long double x)
{
  register long double inout asm("f4") = x;

  asm("sqxbr\t%0,%0" : "+f"(inout));
  asm("# %0" : "+f"(inout));

  return inout;
}

/* Ideally there should be just one `vpdi %v6,%v4,%v6,5`, but the compiler
 * can't optimize it away, because the UNSPEC pattern operates on the whole
 * register.  Using the SUBREG pattern solves this problem, but it's fragile.
 */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v6,%v4,%v6,5\n} 2 } } */
/* { dg-final { scan-assembler-times {\n\tvpdi\t%v4,%v4,%v6,0\n} 2 } } */

int
main (void)
{
  long double x = 0x1.0000000000001p+0L,
	      exp = 1.00000000000000011102230246251564788e+0L;
  assert (sqxbr (x) == exp);
}
