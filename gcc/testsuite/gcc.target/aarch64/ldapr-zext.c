/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
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

#define TEST(name, ldsize, rettype)				\
rettype								\
test_##name (void)						\
{								\
  return atomic_load_explicit (&ldsize, memory_order_acquire);	\
}

/*
**test_u8_u64:
**...
**	ldaprb	w0, \[x[0-9]+\]
**	ret
*/

TEST(u8_u64, u8, unsigned long long)

/*
**test_u16_u64:
**...
**	ldaprh	w0, \[x[0-9]+\]
**	ret
*/

TEST(u16_u64, u16, unsigned long long)

/*
**test_u32_u64:
**...
**	ldapr	w0, \[x[0-9]+\]
**	ret
*/

TEST(u32_u64, u32, unsigned long long)

/*
**test_u8_u32:
**...
**	ldaprb	w0, \[x[0-9]+\]
**	ret
*/

TEST(u8_u32, u8, unsigned)

/*
**test_u16_u32:
**...
**	ldaprh	w0, \[x[0-9]+\]
**	ret
*/
TEST(u16_u32, u16, unsigned)

/*
**test_u8_u16:
**...
**	ldaprb	w0, \[x[0-9]+\]
**	ret
*/
TEST(u8_u16, u8, unsigned short)

