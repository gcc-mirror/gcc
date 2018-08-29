/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (32 / sizeof (TYPE))

#define DEF_LOOP(TYPE, BASE, STEP, SUFFIX)	\
void __attribute__ ((noinline, noclone))	\
loop_##TYPE##_##SUFFIX (TYPE *a)		\
{						\
  for (int i = 0; i < NUM_ELEMS (TYPE); ++i)	\
    a[i] = (BASE) + i * (STEP);			\
}

#define TEST_ALL_UNSIGNED_TYPES(T, BASE, STEP, SUFFIX)	\
  T (uint8_t,  BASE, STEP, SUFFIX)			\
  T (uint16_t, BASE, STEP, SUFFIX)			\
  T (uint32_t, BASE, STEP, SUFFIX)			\
  T (uint64_t, BASE, STEP, SUFFIX)

#define TEST_ALL_SIGNED_TYPES(T, BASE, STEP, SUFFIX)	\
  T (int8_t,  BASE, STEP, SUFFIX)			\
  T (int16_t, BASE, STEP, SUFFIX)			\
  T (int32_t, BASE, STEP, SUFFIX)			\
  T (int64_t, BASE, STEP, SUFFIX)

/* Immediate loops.  */
#define TEST_IMMEDIATE(T)			\
  TEST_ALL_UNSIGNED_TYPES (T, 0, 1, b0s1)	\
  TEST_ALL_SIGNED_TYPES (T, 0, 1, b0s1)		\
  TEST_ALL_UNSIGNED_TYPES (T, 0, 15, b0s15)	\
  TEST_ALL_SIGNED_TYPES (T, 0, 15, b0s15)	\
  TEST_ALL_SIGNED_TYPES (T, 0, -1, b0sm1)	\
  TEST_ALL_SIGNED_TYPES (T, 0, -16, b0sm16)	\
  TEST_ALL_SIGNED_TYPES (T, -16, 1, bm16s1)	\
  TEST_ALL_UNSIGNED_TYPES (T, 15, 1, b15s1)	\
  TEST_ALL_SIGNED_TYPES (T, 15, 1, b15s1)

/* Non-immediate loops.  */
#define TEST_NONIMMEDIATE(T)			\
  TEST_ALL_UNSIGNED_TYPES (T, 0, 16, b0s16)	\
  TEST_ALL_SIGNED_TYPES (T, 0, 16, b0s16)	\
  TEST_ALL_SIGNED_TYPES (T, 0, -17, b0sm17)	\
  TEST_ALL_SIGNED_TYPES (T, -17, 1, bm17s1)	\
  TEST_ALL_UNSIGNED_TYPES (T, 16, 1, b16s1)	\
  TEST_ALL_SIGNED_TYPES (T, 16, 1, b16s1)	\
  TEST_ALL_UNSIGNED_TYPES (T, 16, 16, b16s16)	\
  TEST_ALL_SIGNED_TYPES (T, 16, 16, b16s16)	\
  TEST_ALL_SIGNED_TYPES (T, -17, -17, bm17sm17)

#define TEST_ALL(T) TEST_IMMEDIATE (T) TEST_NONIMMEDIATE (T)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #0, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #0, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #0, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #0, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #-16, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #15, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, #0, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #1\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, w[0-9]+\n} 3 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #0, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #0, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #0, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #0, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #-16, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #15, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, #0, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #1\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, w[0-9]+\n} 3 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #0, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #0, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #0, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #0, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #-16, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #15, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, #0, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #1\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, w[0-9]+\n} 3 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #0, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #0, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #0, #-1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #0, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #-16, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #15, #1\n} 2 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, #0, x[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #1\n} 3 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, x[0-9]+\n} 3 } } */
