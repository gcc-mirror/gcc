/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <arm_sve.h>
#include <stdint.h>

#define INDEX_CONST(TYPE, TY)				\
  sv##TYPE f_##TY##_index_const ()			\
  {							\
    return svindex_##TY (10, 3);			\
  }

#define MULT_INDEX(TYPE, TY)				\
  sv##TYPE f_##TY##_mult_index ()			\
  {							\
    return svmul_x (svptrue_b8 (),			\
		    svindex_##TY (10, 3),		\
		    5);					\
  }

#define ALL_TESTS(TYPE, TY)				\
  INDEX_CONST (TYPE, TY)				\
  MULT_INDEX (TYPE, TY)

ALL_TESTS (uint8_t, u8)
ALL_TESTS (uint16_t, u16)
ALL_TESTS (uint32_t, u32)
ALL_TESTS (uint64_t, u64)
ALL_TESTS (int8_t, s8)
ALL_TESTS (int16_t, s16)
ALL_TESTS (int32_t, s32)
ALL_TESTS (int64_t, s64)

/* { dg-final { scan-tree-dump-times "return \\{ 10, 13, 16, ... \\}" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return \\{ 50, 65, 80, ... \\}" 8 "optimized" } } */
