/* The Shoup modular multiply used by lattice cryptography.  The high-part
   multiply is the only operation Advanced SIMD lacks, so without a 128-bit
   vector form of it the whole loop stays scalar.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -march=armv8.2-a+sve2 -mautovec-preference=asimd-only" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

/*
** mul_mod:
** ...
**	umulh	z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d
** ...
*/
void __attribute__ ((noipa))
mul_mod (uint64_t *restrict dst, const uint64_t *restrict src,
	 uint64_t operand, uint64_t quotient, uint64_t modulus, int count)
{
  for (int i = 0; i < count; ++i)
    {
      uint64_t hi = (uint64_t) (((unsigned __int128) src[i] * quotient) >> 64);
      uint64_t t = operand * src[i] - hi * modulus;
      dst[i] = t >= modulus ? t - modulus : t;
    }
}
