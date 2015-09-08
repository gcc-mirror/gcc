/* { dg-do run } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

extern void abort (void);

#define TESTMETH(BASE, ELTS, STRUCT, SUFFIX)	\
int __attribute__ ((noinline))			\
test_vld##STRUCT##SUFFIX ()			\
{						\
  BASE##_t data[ELTS * STRUCT];			\
  BASE##_t temp[ELTS];				\
  BASE##x##ELTS##x##STRUCT##_t vectors;		\
  int i,j;					\
  for (i = 0; i < STRUCT * ELTS; i++)		\
    data [i] = (BASE##_t) 2*i + 1;		\
  asm volatile ("" : : : "memory");		\
  vectors = vld##STRUCT##SUFFIX (data);		\
  for (i = 0; i < STRUCT; i++)			\
    {						\
      vst1##SUFFIX (temp, vectors.val[i]);	\
      asm volatile ("" : : : "memory");		\
      for (j = 0; j < ELTS; j++)		\
        if (temp[j] != data[i + STRUCT*j])	\
          return 1;				\
    }						\
  return 0;					\
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
VARIANT (float16, 4, STRUCT, _f16)	\
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
VARIANT (float16, 8, STRUCT, q_f16)	\
VARIANT (float32, 4, STRUCT, q_f32)	\
VARIANT (float64, 2, STRUCT, q_f64)

/* Tests of vld2 and vld2q.  */
VARIANTS (TESTMETH, 2)

/* Tests of vld3 and vld3q.  */
VARIANTS (TESTMETH, 3)

/* Tests of vld4 and vld4q.  */
VARIANTS (TESTMETH, 4)

#define CHECK(BASE, ELTS, STRUCT, SUFFIX)	\
  if (test_vld##STRUCT##SUFFIX () != 0)		\
    abort ();

int
main (int argc, char **argv)
{
  VARIANTS (CHECK, 2)
  VARIANTS (CHECK, 3)
  VARIANTS (CHECK, 4)

  return 0;
}

