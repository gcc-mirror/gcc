/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d  -fno-schedule-insns -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

#define INDEX32 uint32_t

#define TEST_LOOP(DATA_TYPE, BITS)                                             \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,            \
		 INDEX##BITS *restrict indices, INDEX##BITS *restrict cond)    \
  {                                                                            \
    for (int i = 0; i < 128; ++i)                                              \
      if (cond[i])                                                             \
	dest[i] += src[indices[i]];                                            \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t, 32)                                                               \
  T (uint8_t, 32)                                                              \
  T (int16_t, 32)                                                              \
  T (uint16_t, 32)                                                             \
  T (_Float16, 32)                                                             \
  T (int32_t, 32)                                                              \
  T (uint32_t, 32)                                                             \
  T (float, 32)                                                                \
  T (int64_t, 32)                                                              \
  T (uint64_t, 32)                                                             \
  T (double, 32)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 11 "vect" } } */
/* { dg-final { scan-tree-dump " \.MASK_LEN_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-assembler-not {vluxei64\.v} } } */
/* { dg-final { scan-assembler-not {vsuxei64\.v} } } */
