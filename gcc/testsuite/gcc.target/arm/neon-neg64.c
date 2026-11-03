/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -save-temps -fno-ipa-icf" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

int64x2_t
negate_intrinsic (int64x2_t value)
{
  return vsubq_s64 (vdupq_n_s64 (0), value);
}

int64x2_t
negate_signed (int64x2_t value)
{
  return -value;
}

uint64x2_t
negate_unsigned (uint64x2_t value)
{
  return -value;
}

int64x2_t
negate_memset (int64x2_t value)
{
  int64x2_t zero;
  __builtin_memset (&zero, 0, sizeof (zero));
  return vsubq_s64 (zero, value);
}

/* { dg-final { scan-assembler-times {vsub\.i64} 4 } } */
