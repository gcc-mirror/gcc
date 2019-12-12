/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256 -fno-tree-loop-distribute-patterns" } */

#define SIZE (15 * 8 + 3)

#define DEF_INDEX_OFFSET(SIGNED, TYPE, ITERTYPE)			\
void __attribute__ ((noinline, noclone))				\
set_##SIGNED##_##TYPE##_##ITERTYPE (SIGNED TYPE *restrict out,		\
				    SIGNED TYPE *restrict in)		\
{									\
  SIGNED ITERTYPE i;							\
  for (i = 0; i < SIZE; i++)						\
  {									\
    out[i] = in[i];							\
  }									\
}									\
void __attribute__ ((noinline, noclone))				\
set_##SIGNED##_##TYPE##_##ITERTYPE##_var (SIGNED TYPE *restrict out,	\
					  SIGNED TYPE *restrict in,	\
					  SIGNED ITERTYPE n)		\
{									\
  SIGNED ITERTYPE i;							\
  for (i = 0; i < n; i++)						\
  {									\
    out[i] = in[i];							\
  }									\
}

#define TEST_TYPE(T, SIGNED, TYPE)		\
  T (SIGNED, TYPE, char)			\
  T (SIGNED, TYPE, short)			\
  T (SIGNED, TYPE, int)				\
  T (SIGNED, TYPE, long)

#define TEST_ALL(T)				\
  TEST_TYPE (T, signed, long)			\
  TEST_TYPE (T, unsigned, long)			\
  TEST_TYPE (T, signed, int)			\
  TEST_TYPE (T, unsigned, int)			\
  TEST_TYPE (T, signed, short)			\
  TEST_TYPE (T, unsigned, short)		\
  TEST_TYPE (T, signed, char)			\
  TEST_TYPE (T, unsigned, char)

TEST_ALL (DEF_INDEX_OFFSET)

/* { dg-final { scan-assembler-times "ld1d\\tz\[0-9\]+.d, p\[0-9\]+/z, \\\[x\[0-9\]+, x\[0-9\]+, lsl 3\\\]" 16 } } */
/* { dg-final { scan-assembler-times "st1d\\tz\[0-9\]+.d, p\[0-9\]+, \\\[x\[0-9\]+, x\[0-9\]+, lsl 3\\\]" 16 } } */
/* { dg-final { scan-assembler-times "ld1w\\tz\[0-9\]+.s, p\[0-9\]+/z, \\\[x\[0-9\]+, x\[0-9\]+, lsl 2\\\]" 16 } } */
/* { dg-final { scan-assembler-times "st1w\\tz\[0-9\]+.s, p\[0-9\]+, \\\[x\[0-9\]+, x\[0-9\]+, lsl 2\\\]" 16 } } */
/* { dg-final { scan-assembler-times "ld1h\\tz\[0-9\]+.h, p\[0-9\]+/z, \\\[x\[0-9\]+, x\[0-9\]+, lsl 1\\\]" 16 } } */
/* { dg-final { scan-assembler-times "st1h\\tz\[0-9\]+.h, p\[0-9\]+, \\\[x\[0-9\]+, x\[0-9\]+, lsl 1\\\]" 16 } } */
/* { dg-final { scan-assembler-times "ld1b\\tz\[0-9\]+.b, p\[0-9\]+/z, \\\[x\[0-9\]+, x\[0-9\]+\\\]" 16 } } */
/* { dg-final { scan-assembler-times "st1b\\tz\[0-9\]+.b, p\[0-9\]+, \\\[x\[0-9\]+, x\[0-9\]+\\\]" 16 } } */
