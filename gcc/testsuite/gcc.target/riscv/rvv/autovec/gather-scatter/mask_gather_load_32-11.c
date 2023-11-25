/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d  -fno-vect-cost-model -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

#define TEST_LOOP(DATA_TYPE, INDEX_TYPE)                                       \
  void __attribute__ ((noinline, noclone))                                     \
  f_##DATA_TYPE##_##INDEX_TYPE (DATA_TYPE *restrict y, DATA_TYPE *restrict x,  \
				INDEX_TYPE *restrict index,                    \
				INDEX_TYPE *restrict cond)                     \
  {                                                                            \
    for (int i = 0; i < 100; ++i)                                              \
      {                                                                        \
	if (cond[i * 2])                                                       \
	  y[i * 2] = x[index[i * 2]] + 1;                                      \
	if (cond[i * 2 + 1])                                                   \
	  y[i * 2 + 1] = x[index[i * 2 + 1]] + 2;                              \
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

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 88 "vect" } } */
/* { dg-final { scan-tree-dump " \.MASK_LEN_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.GATHER_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump-not " \.MASK_GATHER_LOAD" "vect" } } */
/* { dg-final { scan-assembler-not "vluxei64\.v" } } */
/* { dg-final { scan-assembler-not "vsuxei64\.v" } } */
/* { dg-final { scan-assembler-not {vlse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero} } } */
