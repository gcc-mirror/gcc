/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times {(?s);; Function test_srli(?:_add|w_addw|_adduw)_[123] (?:(?!;; Function).)*RISCV_FUSE_SRLI_ADD} 9 "sched2" } } */
/* { dg-final { scan-rtl-dump-not {(?s);; Function test_srli(?:_add|w_addw|_adduw)_(?:4|31) (?:(?!;; Function).)*RISCV_FUSE_SRLI_ADD} "sched2" } } */

typedef long int64_t;
typedef unsigned long uint64_t;
typedef int int32_t;
typedef unsigned int uint32_t;

/* Check each accepted ADD form.  Shift amounts 1, 2 and 3 should fuse;
   4 and 31 should not.  A fixed RTL test covers shifts by zero, which C
   optimization would remove.  */
#define TEST_SRLI_ADD(SHIFT) \
  int64_t \
  test_srli_add_##SHIFT (uint64_t a, int64_t b) \
  { \
    return (int64_t) (a >> SHIFT) + b; \
  } \
  int32_t \
  test_srliw_addw_##SHIFT (uint32_t a, int32_t b) \
  { \
    return (int32_t) (a >> SHIFT) + b; \
  } \
  uint64_t \
  test_srli_adduw_##SHIFT (uint64_t a, uint64_t b) \
  { \
    return (uint64_t) (uint32_t) (a >> SHIFT) + b; \
  }

TEST_SRLI_ADD (1)
TEST_SRLI_ADD (2)
TEST_SRLI_ADD (3)
TEST_SRLI_ADD (4)
TEST_SRLI_ADD (31)

