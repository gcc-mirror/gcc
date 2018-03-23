/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" } */

#include <stdint.h>

#define TEST(NAME, TYPE)			\
 void						\
 NAME##1 (TYPE *x, int n)			\
 {						\
   for (int i = 0; i < n; ++i)			\
     x[i] += 1;					\
 }						\
 TYPE NAME##_array[1024];			\
 void						\
 NAME##2 (void)					\
 {						\
   for (int i = 1; i < 200; ++i)		\
     NAME##_array[i] += 1;			\
 }

TEST (s8, int8_t)
TEST (u8, uint8_t)
TEST (s16, int16_t)
TEST (u16, uint16_t)
TEST (s32, int32_t)
TEST (u32, uint32_t)
TEST (s64, int64_t)
TEST (u64, uint64_t)
TEST (f16, _Float16)
TEST (f32, float)
TEST (f64, double)

/* No scalar memory accesses.  */
/* { dg-final { scan-assembler-not {[wx][0-9]*, \[} } } */
/* 2 for each NAME##1 test, one in the header and one in the main loop
   and 1 for each NAME##2 test, in the main loop only.  */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.b,} 6 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.h,} 9 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.s,} 9 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7]\.d,} 9 } } */
