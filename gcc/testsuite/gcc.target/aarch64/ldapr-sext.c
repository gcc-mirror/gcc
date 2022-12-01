/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
#include <stdatomic.h>

#pragma GCC target "arch=armv8.4-a"

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
**test_s8_s64:
**...
**	ldapursb	x0, \[x[0-9]+\]
**	ret
*/

TEST(s8_s64, s8, long long)

/*
**test_s16_s64:
**...
**	ldapursh	x0, \[x[0-9]+\]
**	ret
*/

TEST(s16_s64, s16, long long)

/*
**test_s32_s64:
**...
**	ldapursw	x0, \[x[0-9]+\]
**	ret
*/

TEST(s32_s64, s32, long long)

/*
**test_s8_s32:
**...
**	ldapursb	w0, \[x[0-9]+\]
**	ret
*/

TEST(s8_s32, s8, int)

/*
**test_s16_s32:
**...
**	ldapursh	w0, \[x[0-9]+\]
**	ret
*/

TEST(s16_s32, s16, int)
