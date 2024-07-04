/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv_zvfh -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define TEST(TYPE, NAME, OP)                                                   \
  void __attribute__ ((noinline, noclone))                                     \
  test_##TYPE##_##NAME (TYPE *__restrict x, TYPE *__restrict y, TYPE z1,       \
			TYPE z2, TYPE *__restrict pred, int n)                 \
  {                                                                            \
    for (int i = 0; i < n; i += 2)                                             \
      {                                                                        \
	x[i] = (pred[i] != 1 ? y[i] OP z1 : y[i]);                             \
	x[i + 1] = (pred[i + 1] != 1 ? y[i + 1] OP z2 : y[i + 1]);             \
      }                                                                        \
  }

#define TEST_TYPE(TYPE)                                                        \
  TEST (TYPE, add, +)                                                          \
  TEST (TYPE, sub, -)                                                          \
  TEST (TYPE, mul, *)                                                          \
  TEST (TYPE, div, /)

#define TEST_TYPE2(TYPE) TEST (TYPE, rem, %)

#define TEST_ALL                                                               \
  TEST_TYPE (int32_t)                                                          \
  TEST_TYPE (uint32_t)                                                         \
  TEST_TYPE (int64_t)                                                          \
  TEST_TYPE (uint64_t)                                                         \
  TEST_TYPE2 (int32_t)                                                         \
  TEST_TYPE2 (uint32_t)                                                        \
  TEST_TYPE2 (int64_t)                                                         \
  TEST_TYPE2 (uint64_t)                                                        \
  TEST_TYPE (_Float16)                                                         \
  TEST_TYPE (float)                                                            \
  TEST_TYPE (double)

TEST_ALL

/* { dg-final { scan-tree-dump-times "\.COND_LEN_DIV" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MOD" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_RDIV" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_ADD" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_ADD" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_SUB" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_SUB" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_MUL" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.COND_LEN_MUL" 3 "optimized" } } */
/* { dg-final { scan-assembler-times {vadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vdivu?\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 4 } } */
/* { dg-final { scan-assembler-times {vrem\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vremu\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfsub\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfmul\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vfdiv\.vv\s+v[0-9]+,v[0-9]+,v[0-9]+,v0.t} 3 } } */
/* { dg-final { scan-assembler-not {\tvf?merge\.v[vxi]m\t} } } */
