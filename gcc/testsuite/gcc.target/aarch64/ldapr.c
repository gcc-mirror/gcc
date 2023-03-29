/* { dg-do compile } */
/* { dg-options "-O1 -std=c99" } */
#include <stdatomic.h>

#pragma GCC target "+rcpc"
atomic_ullong u64;
atomic_llong s64;
atomic_uint u32;
atomic_int s32;
atomic_ushort u16;
atomic_short s16;
atomic_uchar u8;
atomic_schar s8;

#define TEST(size, rettype)					\
rettype								\
test_##size (void)						\
{								\
  return atomic_load_explicit (&size, memory_order_acquire);	\
}								\

TEST(u64, unsigned long long)
TEST(s64, long long)
TEST(u32, unsigned int)
TEST(s32, int)
TEST(u16, unsigned short)
TEST(s16, short)
TEST(u8, unsigned char)
TEST(s8, signed char)

/* { dg-final { scan-assembler-times "ldapr\tx" 2 } } */
/* { dg-final { scan-assembler-times "ldapr\tw" 2 } } */
/* { dg-final { scan-assembler-times "ldaprh\tw" 2 } } */
/* { dg-final { scan-assembler-times "ldaprb\tw" 2 } } */
