/* { dg-do run } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

extern void abort (void);

#define TESTMETH(BASE, ELTS, STRUCT, SUFFIX)		\
int __attribute__ ((noinline))				\
test_vst##STRUCT##SUFFIX ()				\
{							\
  BASE##_t src[ELTS * STRUCT];				\
  BASE##_t dest[ELTS * STRUCT];				\
  BASE##x##ELTS##x##STRUCT##_t vectors;			\
  int i,j;						\
  for (i = 0; i < STRUCT * ELTS; i++)			\
    src [i] = (BASE##_t) 2*i + 1;			\
  for (i = 0; i < STRUCT; i++)				\
    vectors.val[i] = vld1##SUFFIX (&src[i*ELTS]);	\
  asm volatile ("" : : : "memory");			\
  vst##STRUCT##SUFFIX (dest, vectors);			\
  asm volatile ("" : : : "memory");			\
  for (i = 0; i < STRUCT; i++)				\
    {							\
      for (j = 0; j < ELTS; j++)			\
        if (src[i*ELTS + j] != dest[i + STRUCT*j])	\
          return 1;					\
    }							\
  return 0;						\
}

#define VARIANTS(VARIANT, STRUCT)	\
VARIANT (uint8, 8, STRUCT, _u8)		\
VARIANT (uint16, 4, STRUCT, _u16)	\
VARIANT (uint32, 2, STRUCT, _u32)	\
VARIANT (uint64, 1, STRUCT, _u64)	\
VARIANT (int8, 8, STRUCT, _s8)		\
VARIANT (int16, 4, STRUCT, _s16)	\
VARIANT (int32, 2, STRUCT, _s32)	\
VARIANT (int64, 1, STRUCT, _s64)	\
VARIANT (poly8, 8, STRUCT, _p8)		\
VARIANT (poly16, 4, STRUCT, _p16)	\
VARIANT (float32, 2, STRUCT, _f32)	\
VARIANT (float64, 1, STRUCT, _f64)	\
VARIANT (uint8, 16, STRUCT, q_u8)	\
VARIANT (uint16, 8, STRUCT, q_u16)	\
VARIANT (uint32, 4, STRUCT, q_u32)	\
VARIANT (uint64, 2, STRUCT, q_u64)	\
VARIANT (int8, 16, STRUCT, q_s8)	\
VARIANT (int16, 8, STRUCT, q_s16)	\
VARIANT (int32, 4, STRUCT, q_s32)	\
VARIANT (int64, 2, STRUCT, q_s64)	\
VARIANT (poly8, 16, STRUCT, q_p8)	\
VARIANT (poly16, 8, STRUCT, q_p16)	\
VARIANT (float32, 4, STRUCT, q_f32)	\
VARIANT (float64, 2, STRUCT, q_f64)

/* Tests of vst2 and vst2q.  */
VARIANTS (TESTMETH, 2)
/* Tests of vst3 and vst3q.  */
VARIANTS (TESTMETH, 3)
/* Tests of vst4 and vst4q.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, ELTS, STRUCT, SUFFIX)	\
  if (test_vst##STRUCT##SUFFIX () != 0)		\
    abort ();

int
main (int argc, char **argv)
{
  VARIANTS (CHECK, 2)
  VARIANTS (CHECK, 3)
  VARIANTS (CHECK, 4)
  return 0;
}
