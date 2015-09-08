/* Test vld1 and vst1 maintain consistent indexing.  */
/* { dg-do run } */
/* { dg-options "-O3" } */
#include <arm_neon.h>

extern void abort (void);

#define TESTMETH(TYPE, NUM, BASETYPE, SUFFIX)	\
int __attribute__ ((noinline))			\
test_vld1_vst1##SUFFIX ()			\
{						\
  TYPE vec;					\
  int i = 0;					\
  BASETYPE src[NUM];				\
  BASETYPE dest[NUM];				\
  for (i = 0; i < NUM; i++)			\
    src[i] = 2*i + 1;				\
  asm volatile ("":::"memory");			\
  vec = vld1 ## SUFFIX (src);			\
  asm volatile ("":::"memory");			\
  vst1 ## SUFFIX (dest, vec);			\
  asm volatile ("":::"memory");			\
  for (i = 0; i < NUM; i++)			\
    if (src[i] != dest[i])			\
      return 1;					\
  return 0;					\
}

#define VARIANTS(THING)			\
THING (int8x8_t, 8, int8_t, _s8)	\
THING (uint8x8_t, 8, uint8_t, _u8)	\
THING (int16x4_t, 4, int16_t, _s16)	\
THING (uint16x4_t, 4, uint16_t, _u16)	\
THING (float16x4_t, 4, float16_t, _f16)	\
THING (int32x2_t, 2, int32_t, _s32)	\
THING (uint32x2_t, 2, uint32_t, _u32)	\
THING (float32x2_t, 2, float32_t, _f32) \
THING (int8x16_t, 16, int8_t, q_s8)	\
THING (uint8x16_t, 16, uint8_t, q_u8)	\
THING (int16x8_t, 8, int16_t, q_s16)	\
THING (uint16x8_t, 8, uint16_t, q_u16)	\
THING (float16x8_t, 8, float16_t, q_f16)\
THING (int32x4_t, 4, int32_t, q_s32)	\
THING (uint32x4_t, 4, uint32_t, q_u32)	\
THING (float32x4_t, 4, float32_t, q_f32)\
THING (int64x2_t, 2, int64_t, q_s64)	\
THING (uint64x2_t, 2, uint64_t, q_u64)	\
THING (float64x2_t, 2, float64_t, q_f64)

VARIANTS (TESTMETH)

#define DOTEST(TYPE, NUM, BASETYPE, SUFFIX)	\
  if (test_vld1_vst1##SUFFIX ())		\
    abort ();

int
main ()
{
  VARIANTS (DOTEST);
  return 0;
}
