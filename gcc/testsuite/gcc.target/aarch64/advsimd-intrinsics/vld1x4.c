/* We haven't implemented these intrinsics for arm yet.  */
/* { dg-do run } */
/* { dg-skip-if "unimplemented" { arm*-*-* } } */
/* { dg-options "-O3" } */

#include <stdbool.h>
#include <arm_neon.h>
#include "arm-neon-ref.h"

extern void abort (void);

#define TESTMETH(BASE, ELTS, SUFFIX)			\
int __attribute__ ((noinline))				\
test_vld1##SUFFIX##_x4 ()				\
{							\
  BASE##_t data[ELTS * 4];				\
  BASE##_t temp[ELTS * 4];				\
  BASE##x##ELTS##x##4##_t vectors;			\
  int i,j;						\
  for (i = 0; i < ELTS * 4; i++)			\
    data [i] = (BASE##_t) 4*i;				\
  asm volatile ("" : : : "memory");			\
  vectors = vld1##SUFFIX##_x4 (data);			\
  vst1##SUFFIX (temp, vectors.val[0]);			\
  vst1##SUFFIX (&temp[ELTS], vectors.val[1]);		\
  vst1##SUFFIX (&temp[ELTS * 2], vectors.val[2]);	\
  vst1##SUFFIX (&temp[ELTS * 3], vectors.val[3]);	\
  asm volatile ("" : : : "memory");			\
  for (j = 0; j < ELTS * 4; j++)			\
    if (temp[j] != data[j])				\
      return 1;						\
  return 0;						\
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
VARIANT (poly64, 1, _p64)	\
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
VARIANT (poly64, 2, q_p64)	\
VARIANT (float16, 8, q_f16)	\
VARIANT (float32, 4, q_f32)

#ifdef __aarch64__
#define VARIANTS(VARIANT) VARIANTS_1(VARIANT)	\
VARIANT (float64, 1, _f64)			\
VARIANT (float64, 2, q_f64)
#else
#define VARIANTS(VARIANT) VARIANTS_1(VARIANT)
#endif

/* Tests of vld1_x4 and vld1q_x4.  */
VARIANTS (TESTMETH)

#define CHECKS(BASE, ELTS, SUFFIX)	\
  if (test_vld1##SUFFIX##_x4 () != 0) {	\
    fprintf (stderr, "test_vld1" #SUFFIX "_x4 failed\n"); \
    failed = true; \
  }

int
main (int argc, char **argv)
{
  bool failed = false;
  VARIANTS (CHECKS)

  return (failed) ? 1 : 0;
}
