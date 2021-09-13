/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch --save-temps" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>
#include <stdint.h>

__attribute__ ((noipa)) static long double
sqxbr (long double x)
{
  register long double in asm("f0") = x;
  register long double out asm("f1");

  asm("sqxbr\t%0,%1" : "=f"(out) : "f"(in));
  asm("# %0" : "+f"(out));

  return out;
}

/* Ideally `vmrlg %v3,%v1,%v3` should be optimized away, but the compiler
 * can't do it, because the UNSPEC pattern operates on the whole register.
 * Using the SUBREG pattern solves this problem, but it's fragile.
 */
/* { dg-final { scan-assembler-times {\n\tvmrlg\t%v2,%v0,%v2\n} 1 } } */
/* { dg-final { scan-assembler-times {\n\tvmrhg\t%v1,%v1,%v3\n} 2 } } */
/* { dg-final { scan-assembler-times {\n\tvmrlg\t%v3,%v1,%v3\n} 1 } } */

int
main (void)
{
  long double x = 0x1.0000000000001p+0L,
	      exp = 1.00000000000000011102230246251564788e+0L;
  assert (sqxbr (x) == exp);
}
