/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define TEST(DATA_TYPE, OTHER_TYPE, NAME, OP)                                  \
  void __attribute__ ((noinline, noclone))                                     \
  test_##DATA_TYPE##_##OTHER_TYPE##_##NAME (DATA_TYPE *__restrict x,           \
					    DATA_TYPE *__restrict y,           \
					    DATA_TYPE z1, DATA_TYPE z2,        \
					    DATA_TYPE *__restrict pred,        \
					    OTHER_TYPE *__restrict foo, int n) \
  {                                                                            \
    for (int i = 0; i < n; i += 2)                                             \
      {                                                                        \
	x[i] = (pred[i] != 1 ? y[i] OP z1 : y[i]);                             \
	x[i + 1] = (pred[i + 1] != 1 ? y[i + 1] OP z2 : y[i + 1]);             \
	foo[i] += 1;                                                           \
	foo[i + 1] += 2;                                                       \
      }                                                                        \
  }

#define TEST_TYPE(DATA_TYPE, OTHER_TYPE)                                       \
  TEST (DATA_TYPE, OTHER_TYPE, add, +)                                         \
  TEST (DATA_TYPE, OTHER_TYPE, sub, -)                                         \
  TEST (DATA_TYPE, OTHER_TYPE, mul, *)                                         \
  TEST (DATA_TYPE, OTHER_TYPE, div, /)

#define TEST_TYPE2(DATA_TYPE, OTHER_TYPE) TEST (DATA_TYPE, OTHER_TYPE, rem, %)

#define TEST_ALL                                                               \
  TEST_TYPE (int32_t, int8_t)                                                  \
  TEST_TYPE (int32_t, int16_t)                                                 \
  TEST_TYPE (uint32_t, int8_t)                                                 \
  TEST_TYPE (uint32_t, int16_t)                                                \
  TEST_TYPE (int64_t, int8_t)                                                  \
  TEST_TYPE (int64_t, int16_t)                                                 \
  TEST_TYPE (int64_t, int32_t)                                                 \
  TEST_TYPE (uint64_t, int8_t)                                                 \
  TEST_TYPE (uint64_t, int16_t)                                                \
  TEST_TYPE (uint64_t, int32_t)                                                \
  TEST_TYPE2 (int32_t, int8_t)                                                 \
  TEST_TYPE2 (int32_t, int16_t)                                                \
  TEST_TYPE2 (uint32_t, int8_t)                                                \
  TEST_TYPE2 (uint32_t, int16_t)                                               \
  TEST_TYPE2 (int64_t, int8_t)                                                 \
  TEST_TYPE2 (int64_t, int16_t)                                                \
  TEST_TYPE2 (int64_t, int32_t)                                                \
  TEST_TYPE2 (uint64_t, int8_t)                                                \
  TEST_TYPE2 (uint64_t, int16_t)                                               \
  TEST_TYPE2 (uint64_t, int32_t)                                               \
  TEST_TYPE (_Float16, int8_t)                                                 \
  TEST_TYPE (float, int8_t)                                                    \
  TEST_TYPE (float, int16_t)                                                   \
  TEST_TYPE (double, int8_t)                                                   \
  TEST_TYPE (double, int16_t)                                                  \
  TEST_TYPE (double, int32_t)

TEST_ALL

/* { dg-final { scan-tree-dump-times "\.COND_ADD" 40 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_SUB" 40 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_MUL" 40 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 22 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 22 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MUL" 22 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_DIV" 40 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 40 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_RDIV" 22 "optimized" } } */
/* { dg-final { scan-assembler-times {vadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+} 114 } } */
/* { dg-final { scan-assembler-times {vsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 40 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 40 } } */
/* { dg-final { scan-assembler-times {vdivu?\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 40 } } */
/* { dg-final { scan-assembler-times {vrem\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 20 } } */
/* { dg-final { scan-assembler-times {vremu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 20 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 22 } } */
/* { dg-final { scan-assembler-times {vfsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 22 } } */
/* { dg-final { scan-assembler-times {vfmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 22 } } */
/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 22 } } */
/* { dg-final { scan-assembler-times {\tvf?merge\.v[vxi]m\t} 14 } } */
