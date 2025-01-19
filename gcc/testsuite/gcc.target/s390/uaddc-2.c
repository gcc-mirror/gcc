/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch -save-temps -fdump-tree-optimized" }  */
/* { dg-final { scan-tree-dump-times "\\.UADDC \\(" 6 "optimized" } } */
/* { dg-final { scan-assembler-times "\\talcr\\t" 6 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "\\talcr\\t" 3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "\\talcgr\\t" 3 { target lp64 } } } */

#define TEST(T, V, OP) \
  unsigned T \
  uaddc_##T##_##V (unsigned T x, unsigned T y, _Bool carry_in, _Bool *carry_out) \
  { \
    unsigned T r; \
    _Bool c1 = __builtin_add_overflow (x, y, &r); \
    _Bool c2 = __builtin_add_overflow (r, carry_in, &r); \
    *carry_out = c1 OP c2; \
    return r; \
  }

TEST(int, 1, |)
TEST(int, 2, ||)
TEST(int, 3, ^)

TEST(long, 1, |)
TEST(long, 2, ||)
TEST(long, 3, ^)
