/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvfh -mabi=lp64d  -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

#define TEST_LOOP(DATA_TYPE)                                                   \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE (DATA_TYPE *restrict dest, DATA_TYPE *restrict *src)           \
  {                                                                            \
    for (int i = 0; i < 128; ++i)                                              \
      dest[i] += *src[i];                                                      \
  }

#define TEST_ALL(T)                                                            \
  T (int8_t)                                                                   \
  T (uint8_t)                                                                  \
  T (int16_t)                                                                  \
  T (uint16_t)                                                                 \
  T (_Float16)                                                                 \
  T (int32_t)                                                                  \
  T (uint32_t)                                                                 \
  T (float)                                                                    \
  T (int64_t)                                                                  \
  T (uint64_t)                                                                 \
  T (double)

TEST_ALL (TEST_LOOP)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 11 "vect" } } */
/* { dg-final { scan-tree-dump " \.MASK_LEN_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_GATHER_LOAD" "vect" } } */
