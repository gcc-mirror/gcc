/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -march=clearwaterforest" } */

#include <stdint.h>

/* 64x64 -> high 64 open-coded with 32-bit halves, as in SEAL's
   multiply_uint64_hw64_generic.  */
static inline uint64_t
mulhi64 (uint64_t a, uint64_t b)
{
  uint64_t a_lo = a & 0xffffffff, b_lo = b & 0xffffffff;
  a >>= 32;
  b >>= 32;
  uint64_t mid1 = a * b_lo;
  uint64_t mid = mid1 + b * a_lo;
  uint64_t left = a * b + ((uint64_t) (mid < mid1) << 32);
  uint64_t right = a_lo * b_lo;
  uint64_t tmp = (right >> 32) + (mid & 0xffffffff);
  return left + (mid >> 32) + (tmp >> 32);
}

struct mod_operand { uint64_t operand, quotient; };
struct arith { uint64_t modulus, two_times_modulus; };

/* NTT butterfly with Barrett multiply_uint_mod_lazy, unrolled by 4,
   reduced from 750.sealcrypto_r.  */
void
ntt_butterfly (const struct arith *a, uint64_t *x, uint64_t *y,
	       struct mod_operand r, unsigned long gap)
{
  for (unsigned long j = 0; j < gap; j += 4)
    {
#define BFLY							\
      {								\
	uint64_t u = *x >= a->two_times_modulus			\
		     ? *x - a->two_times_modulus : *x;		\
	uint64_t v = r.operand * *y				\
		     - mulhi64 (*y, r.quotient) * a->modulus;	\
	*x++ = u + v;						\
	*y++ = u + a->two_times_modulus - v;			\
      }
      BFLY BFLY BFLY BFLY
    }
}

/* With 256-bit ops split into two 128-bit uops, vectorizing the
   64bit unsigned highpart multiply with 4 vpmuludq plus the emulated
   64bit multiplies is slower than the scalar mulx/imul sequence.  */
/* { dg-final { scan-assembler-times "pmuludq" 0 } } */
