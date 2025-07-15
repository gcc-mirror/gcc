/* { dg-do compile } */
/* { dg-options "-O2 -std=c99" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdatomic.h>
#include <stdint.h>

#pragma GCC target "arch=armv8.8-a"

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


/*
** test_u8_u64:
**	...
**	ldapurb	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u8_u64, u8[1], uint64_t)

/*
** test_u16_u64:
**	...
**	ldapurh	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u16_u64, u16, uint64_t)

/*
**test_u32_u64:
**	...
**	ldapur	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u32_u64, u32, uint64_t)

/*
**test_u64_u64:
**	...
**	ldapur	x[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u64_u64, u64, uint64_t)

/*
**test_u8_u32:
**	...
**	ldapurb	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u8_u32, u8[1], uint32_t)

/*
**test_u16_u32:
**	...
**	ldapurh	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u16_u32, u16, uint32_t)

/*
**test_u32_u32:
**	...
**	ldapur	w[0-9]+, \[x[0-9]+, [0-9]+\]
**	ret
*/
TEST(u32_u32, u32, uint32_t)