/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_SEL_IMM(TYPE, SUFFIX, IMM)					\
void									\
sel_##TYPE##_##SUFFIX (TYPE *restrict a, TYPE *restrict b, int n)	\
{									\
  for (int i = 0; i < n; i++)						\
    a[i] = b[i] != 0 ? IMM : 0;						\
}

#define DEF_SEL_VAR(TYPE)						\
void									\
sel_##TYPE##_var (TYPE *restrict a, TYPE *restrict b, TYPE val, int n)	\
{									\
  for (int i = 0; i < n; i++)						\
    a[i] = b[i] != 0 ? val : 0;						\
}

#define TEST_TYPE8(TYPE)			\
  DEF_SEL_VAR (TYPE)				\
  DEF_SEL_IMM (TYPE, m128, -128)		\
  DEF_SEL_IMM (TYPE, m127, -127)		\
  DEF_SEL_IMM (TYPE, 2, 2)			\
  DEF_SEL_IMM (TYPE, 127, 127)

#define TEST_TYPE16(TYPE)			\
  TEST_TYPE8 (TYPE)				\
  DEF_SEL_IMM (TYPE, m32768, -32768)		\
  DEF_SEL_IMM (TYPE, m32767, -32767)		\
  DEF_SEL_IMM (TYPE, m32512, -32512)		\
  DEF_SEL_IMM (TYPE, m32511, -32511)		\
  DEF_SEL_IMM (TYPE, m256, -256)		\
  DEF_SEL_IMM (TYPE, m255, -255)		\
  DEF_SEL_IMM (TYPE, m129, -129)		\
  DEF_SEL_IMM (TYPE, 128, 128)			\
  DEF_SEL_IMM (TYPE, 256, 256)			\
  DEF_SEL_IMM (TYPE, 32511, 32511)		\
  DEF_SEL_IMM (TYPE, 32512, 32512)		\
  DEF_SEL_IMM (TYPE, 32767, 32767)

#define TEST_TYPE32(TYPE)			\
  TEST_TYPE16 (TYPE)				\
  DEF_SEL_IMM (TYPE, m65536, -65536)		\
  DEF_SEL_IMM (TYPE, m32769, -32769)		\
  DEF_SEL_IMM (TYPE, 32768, 32768)

TEST_TYPE8 (int8_t)
TEST_TYPE16 (int16_t)
TEST_TYPE32 (int32_t)
TEST_TYPE32 (int64_t)

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z, #-128\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z, #-127\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z, #127\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #-32768\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #-32512\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #-256\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #-128\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #-127\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #2\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #127\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #256\n} 3 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.[hsd], p[0-9]+/z, #32512\n} 3 } } */
