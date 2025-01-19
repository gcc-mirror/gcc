/* We haven't implemented these intrinsics for arm yet.  */
/* { dg-do run } */
/* { dg-skip-if "unimplemented" { arm*-*-* } } */
/* { dg-options "-O3" } */

#include <arm_neon.h>
#include "arm-neon-ref.h"

extern void abort (void);

#define TESTMETH(BASE, ELTS, SUFFIX)	\
int __attribute__ ((noinline))			\
test_vst1##SUFFIX##_x2 ()			\
{						\
  BASE##_t data[ELTS * 2];			\
  BASE##_t temp[ELTS * 2];			\
  BASE##x##ELTS##x##2##_t vectors;		\
  int i,j;					\
  for (i = 0; i < ELTS * 2; i++)		\
    data [i] = CONVERT (BASE##_t, 2*i);		\
  asm volatile ("" : : : "memory");		\
  vectors.val[0] = vld1##SUFFIX (data);		\
  vectors.val[1] = vld1##SUFFIX (&data[ELTS]);	\
  vst1##SUFFIX##_x2 (temp, vectors);		\
  asm volatile ("" : : : "memory");		\
  for (j = 0; j < ELTS * 2; j++)		\
    if (!BITEQUAL (temp[j], data[j]))		\
      return 1;					\
  return 0;					\
}

#define VARIANTS_1(VARIANT)	\
VARIANT (uint8, 8, _u8)		\
VARIANT (uint16, 4, _u16)	\
VARIANT (uint32, 2, _u32)	\
VARIANT (uint64, 1, _u64)	\
VARIANT (int8, 8, _s8)		\
VARIANT (int16, 4, _s16)	\
VARIANT (int32, 2, _s32)	\
VARIANT (int64, 1, _s64)	\
VARIANT (poly8, 8, _p8)		\
VARIANT (poly16, 4, _p16)	\
VARIANT (float16, 4, _f16)	\
VARIANT (float32, 2, _f32)	\
VARIANT (uint8, 16, q_u8)	\
VARIANT (uint16, 8, q_u16)	\
VARIANT (uint32, 4, q_u32)	\
VARIANT (uint64, 2, q_u64)	\
VARIANT (int8, 16, q_s8)	\
VARIANT (int16, 8, q_s16)	\
VARIANT (int32, 4, q_s32)	\
VARIANT (int64, 2, q_s64)	\
VARIANT (poly8, 16, q_p8)	\
VARIANT (poly16, 8, q_p16)	\
VARIANT (float16, 8, q_f16)	\
VARIANT (float32, 4, q_f32)

#ifdef __aarch64__
#define VARIANTS(VARIANT) VARIANTS_1(VARIANT)	\
VARIANT (mfloat8, 8, _mf8)			\
VARIANT (mfloat8, 16, q_mf8)			\
VARIANT (float64, 1, _f64)			\
VARIANT (float64, 2, q_f64)
#else
#define VARIANTS(VARIANT) VARIANTS_1(VARIANT)
#endif

/* Tests of vst1_x2 and vst1q_x2.  */
VARIANTS (TESTMETH)

#define CHECKS(BASE, ELTS, SUFFIX)	\
  if (test_vst1##SUFFIX##_x2 () != 0)	\
    fprintf (stderr, "test_vst1##SUFFIX##_x2"), __builtin_abort ();

int
main (int argc, char **argv)
{
  VARIANTS (CHECKS)

  return 0;
}

