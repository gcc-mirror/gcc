/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d  -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

#define INDEX8 uint8_t

#define TEST_LOOP(DATA_TYPE, BITS)                                             \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict src,            \
		 INDEX##BITS *restrict indices)                                \
  {                                                                            \
    for (int i = 0; i < 128; ++i)                                              \
      dest[indices[i]] = src[i] + 1;                                           \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t, 8)                                                                \
  T (uint8_t, 8)                                                               \
  T (int16_t, 8)                                                               \
  T (uint16_t, 8)                                                              \
  T (_Float16, 8)                                                              \
  T (int32_t, 8)                                                               \
  T (uint32_t, 8)                                                              \
  T (float, 8)                                                                 \
  T (int64_t, 8)                                                               \
  T (uint64_t, 8)                                                              \
  T (double, 8)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 11 "vect" } } */
/* { dg-final { scan-tree-dump " \.MASK_LEN_SCATTER_STORE" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.SCATTER_STORE" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_SCATTER_STORE" "vect" } } */
/* { dg-final { scan-assembler-not {vluxei64\.v} } } */
/* { dg-final { scan-assembler-not {vsuxei64\.v} } } */
