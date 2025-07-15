/* { dg-do compile } */
/* { dg-options "-O2 -std=c99 -moverride=tune=avoid_ldapur" } */

#include <stdatomic.h>
#include <stdint.h>

#pragma GCC target "arch=armv8.8-a"
/* LDAPUR is only avoided for armv8.4 to armv8.7. This checks for the working
of avoid_ldapur flag. */

/* { dg-final { scan-assembler-not "ldapur\t" } } */

atomic_ullong u64;
atomic_uint u32;
atomic_ushort u16;
atomic_uchar u8[2]; /* Force an offset for u8 */

#define TEST(name, ldsize, rettype)				\
rettype								\
test_##name (void)						\
{								\
  return atomic_load_explicit (&ldsize, memory_order_acquire);	\
}								\

TEST(u8_u64, u8[1], uint64_t)
TEST(u16_u64, u16, uint64_t)
TEST(u32_u64, u32, uint64_t)
TEST(u64_u64, u64, uint64_t)
TEST(u8_u32, u8[1], uint32_t)
TEST(u16_u32, u16, uint32_t)
TEST(u32_u32, u32, uint32_t)

/* { dg-final { scan-assembler-times "ldapr\t" 3 } } */
/* { dg-final { scan-assembler-times "ldaprh\t" 2 } } */
/* { dg-final { scan-assembler-times "ldaprb\t" 2 } } */


