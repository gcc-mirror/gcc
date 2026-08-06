/* Advanced SIMD has no 64-bit high-part multiply, but SVE does, and the two
   register files overlap.  Check that the SVE instruction is used for a
   128-bit vector high-part multiply when the vectoriser is restricted to
   Advanced SIMD modes.  */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -march=armv8.2-a+sve -mautovec-preference=asimd-only" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

/*
** mulh_s64:
** ...
**	smulh	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d
** ...
*/
void __attribute__ ((noipa))
mulh_s64 (int64_t *restrict dst, int64_t *restrict a, int64_t *restrict b,
	  int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = (int64_t) (((__int128) a[i] * b[i]) >> 64);
}

/*
** mulh_u64:
** ...
**	umulh	z[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d
** ...
*/
void __attribute__ ((noipa))
mulh_u64 (uint64_t *restrict dst, uint64_t *restrict a, uint64_t *restrict b,
	  int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = (uint64_t) (((unsigned __int128) a[i] * b[i]) >> 64);
}
