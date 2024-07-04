/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -ffast-math -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define TEST(DATA_TYPE, PRED_TYPE, NAME, OP)                                   \
  void __attribute__ ((noinline, noclone))                                     \
  test_##DATA_TYPE##_##PRED_TYPE##_##NAME (DATA_TYPE *__restrict x,            \
					   DATA_TYPE *__restrict y,            \
					   DATA_TYPE *__restrict z,            \
					   PRED_TYPE *__restrict pred, int n)  \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      x[i] = pred[i] != 1 ? y[i] OP z[i] : y[i];                               \
  }

#define TEST_TYPE(DATA_TYPE, PRED_TYPE)                                        \
  TEST (DATA_TYPE, PRED_TYPE, add, +)                                          \
  TEST (DATA_TYPE, PRED_TYPE, sub, -)                                          \
  TEST (DATA_TYPE, PRED_TYPE, mul, *)                                          \
  TEST (DATA_TYPE, PRED_TYPE, div, /)

#define TEST_TYPE2(DATA_TYPE, PRED_TYPE) TEST (DATA_TYPE, PRED_TYPE, rem, %)

#define TEST_ALL                                                               \
  TEST_TYPE (int32_t, int8_t)                                                  \
  TEST_TYPE (uint32_t, int8_t)                                                 \
  TEST_TYPE (int32_t, int16_t)                                                 \
  TEST_TYPE (uint32_t, int16_t)                                                \
  TEST_TYPE (int64_t, int8_t)                                                  \
  TEST_TYPE (uint64_t, int8_t)                                                 \
  TEST_TYPE (int64_t, int16_t)                                                 \
  TEST_TYPE (uint64_t, int16_t)                                                \
  TEST_TYPE (int64_t, int32_t)                                                 \
  TEST_TYPE (uint64_t, int32_t)                                                \
  TEST_TYPE2 (int32_t, int8_t)                                                 \
  TEST_TYPE2 (uint32_t, int8_t)                                                \
  TEST_TYPE2 (int32_t, int16_t)                                                \
  TEST_TYPE2 (uint32_t, int16_t)                                               \
  TEST_TYPE2 (int64_t, int8_t)                                                 \
  TEST_TYPE2 (uint64_t, int8_t)                                                \
  TEST_TYPE2 (int64_t, int16_t)                                                \
  TEST_TYPE2 (uint64_t, int16_t)                                               \
  TEST_TYPE2 (int64_t, int32_t)                                                \
  TEST_TYPE2 (uint64_t, int32_t)                                               \
  TEST_TYPE (_Float16, int8_t)                                                 \
  TEST_TYPE (float, int8_t)                                                    \
  TEST_TYPE (float, int16_t)                                                   \
  TEST_TYPE (double, int8_t)                                                   \
  TEST_TYPE (double, int16_t)                                                  \
  TEST_TYPE (double, int32_t)

TEST_ALL

/* { dg-final { scan-tree-dump-times "\.COND_LEN_DIV" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_ADD" 16 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_SUB" 16 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_MUL" 16 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_RDIV" 6 "optimized" } } */
/* { dg-final { scan-assembler-times {vadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 10 } } */
/* { dg-final { scan-assembler-times {vsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 10 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 10 } } */
/* { dg-final { scan-assembler-times {vdivu?\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 10 } } */
/* { dg-final { scan-assembler-times {vrem\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 5 } } */
/* { dg-final { scan-assembler-times {vremu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 5 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 6 } } */
/* { dg-final { scan-assembler-times {vfsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 6 } } */
/* { dg-final { scan-assembler-times {vfmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 6 } } */
/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 6 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
