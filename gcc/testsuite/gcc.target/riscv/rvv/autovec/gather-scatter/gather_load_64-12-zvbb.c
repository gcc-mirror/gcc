/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-fno-vect-cost-model -fdump-tree-vect-details -mrvv-max-lmul=m4" } */

#include <stdint-gcc.h>

#define TEST_LOOP(DATA_TYPE, INDEX_TYPE)                                       \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE##_##INDEX_TYPE (DATA_TYPE *restrict y, DATA_TYPE *restrict x,  \
				INDEX_TYPE *restrict index)                    \
  {                                                                            \
    for (int i = 0; i < 100; ++i)                                              \
      {                                                                        \
	y[i * 2] = x[index[i * 2]] + 1;                                        \
	y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;                                \
      }                                                                        \
  }

TEST_LOOP (int8_t, int8_t)
TEST_LOOP (uint8_t, int8_t)
TEST_LOOP (int16_t, int8_t)
TEST_LOOP (uint16_t, int8_t)
TEST_LOOP (int32_t, int8_t)
TEST_LOOP (uint32_t, int8_t)
TEST_LOOP (int64_t, int8_t)
TEST_LOOP (uint64_t, int8_t)
TEST_LOOP (_Float16, int8_t)
TEST_LOOP (float, int8_t)
TEST_LOOP (double, int8_t)
TEST_LOOP (int8_t, int16_t)
TEST_LOOP (uint8_t, int16_t)
TEST_LOOP (int16_t, int16_t)
TEST_LOOP (uint16_t, int16_t)
TEST_LOOP (int32_t, int16_t)
TEST_LOOP (uint32_t, int16_t)
TEST_LOOP (int64_t, int16_t)
TEST_LOOP (uint64_t, int16_t)
TEST_LOOP (_Float16, int16_t)
TEST_LOOP (float, int16_t)
TEST_LOOP (double, int16_t)
TEST_LOOP (int8_t, int32_t)
TEST_LOOP (uint8_t, int32_t)
TEST_LOOP (int16_t, int32_t)
TEST_LOOP (uint16_t, int32_t)
TEST_LOOP (int32_t, int32_t)
TEST_LOOP (uint32_t, int32_t)
TEST_LOOP (int64_t, int32_t)
TEST_LOOP (uint64_t, int32_t)
TEST_LOOP (_Float16, int32_t)
TEST_LOOP (float, int32_t)
TEST_LOOP (double, int32_t)
TEST_LOOP (int8_t, int64_t)
TEST_LOOP (uint8_t, int64_t)
TEST_LOOP (int16_t, int64_t)
TEST_LOOP (uint16_t, int64_t)
TEST_LOOP (int32_t, int64_t)
TEST_LOOP (uint32_t, int64_t)
TEST_LOOP (int64_t, int64_t)
TEST_LOOP (uint64_t, int64_t)
TEST_LOOP (_Float16, int64_t)
TEST_LOOP (float, int64_t)
TEST_LOOP (double, int64_t)
TEST_LOOP (int8_t, uint8_t)
TEST_LOOP (uint8_t, uint8_t)
TEST_LOOP (int16_t, uint8_t)
TEST_LOOP (uint16_t, uint8_t)
TEST_LOOP (int32_t, uint8_t)
TEST_LOOP (uint32_t, uint8_t)
TEST_LOOP (int64_t, uint8_t)
TEST_LOOP (uint64_t, uint8_t)
TEST_LOOP (_Float16, uint8_t)
TEST_LOOP (float, uint8_t)
TEST_LOOP (double, uint8_t)
TEST_LOOP (int8_t, uint16_t)
TEST_LOOP (uint8_t, uint16_t)
TEST_LOOP (int16_t, uint16_t)
TEST_LOOP (uint16_t, uint16_t)
TEST_LOOP (int32_t, uint16_t)
TEST_LOOP (uint32_t, uint16_t)
TEST_LOOP (int64_t, uint16_t)
TEST_LOOP (uint64_t, uint16_t)
TEST_LOOP (_Float16, uint16_t)
TEST_LOOP (float, uint16_t)
TEST_LOOP (double, uint16_t)
TEST_LOOP (int8_t, uint32_t)
TEST_LOOP (uint8_t, uint32_t)
TEST_LOOP (int16_t, uint32_t)
TEST_LOOP (uint16_t, uint32_t)
TEST_LOOP (int32_t, uint32_t)
TEST_LOOP (uint32_t, uint32_t)
TEST_LOOP (int64_t, uint32_t)
TEST_LOOP (uint64_t, uint32_t)
TEST_LOOP (_Float16, uint32_t)
TEST_LOOP (float, uint32_t)
TEST_LOOP (double, uint32_t)
TEST_LOOP (int8_t, uint64_t)
TEST_LOOP (uint8_t, uint64_t)
TEST_LOOP (int16_t, uint64_t)
TEST_LOOP (uint16_t, uint64_t)
TEST_LOOP (int32_t, uint64_t)
TEST_LOOP (uint32_t, uint64_t)
TEST_LOOP (int64_t, uint64_t)
TEST_LOOP (uint64_t, uint64_t)
TEST_LOOP (_Float16, uint64_t)
TEST_LOOP (float, uint64_t)
TEST_LOOP (double, uint64_t)

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 80 "vect" { target { ! riscv_zvfh } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 88 "vect" { target riscv_zvfh } } } */
/* { dg-final { scan-tree-dump " \.MASK_LEN_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-assembler "vwsll.vi" } } */
