/* { dg-options "-O2 -Wpsabi" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdarg.h>

#define ALIGN __attribute__((aligned(16)))

typedef __uint128_t u128_4 __attribute__((aligned(4)));
typedef __uint128_t u128_8 __attribute__((aligned(8)));
typedef __uint128_t u128_16 __attribute__((aligned(16)));
typedef __uint128_t u128_32 __attribute__((aligned(32)));
typedef __uint128_t u128;

typedef __UINT64_TYPE__ u64_4 __attribute__((aligned(4)));
typedef __UINT64_TYPE__ u64_8 __attribute__((aligned(8)));
typedef __UINT64_TYPE__ u64_16 __attribute__((aligned(16)));
typedef __UINT64_TYPE__ u64_32 __attribute__((aligned(32)));
typedef __UINT64_TYPE__ u64;

enum class ALIGN e128_4 : u128_4 { A };
enum class ALIGN e128_8 : u128_8 { A };
enum class ALIGN e128_16 : u128_16 { A };
enum class ALIGN e128_32 : u128_32 { A };
enum class ALIGN e128 : u128 { A };

enum class ALIGN e64_4 : u64_4 { A };
enum class ALIGN e64_8 : u64_8 { A };
enum class ALIGN e64_16 : u64_16 { A };
enum class ALIGN e64_32 : u64_32 { A };
enum class ALIGN e64 : u64 { A };

extern "C" {

/*
** reg_e128_4:
** (
**	mov	x1, x3
**	mov	x0, x2
** |
**	mov	x0, x2
**	mov	x1, x3
** )
**	ret
*/
e128_4 reg_e128_4 (int x, e128_4 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e128_8:
** (
**	mov	x1, x3
**	mov	x0, x2
** |
**	mov	x0, x2
**	mov	x1, x3
** )
**	ret
*/
e128_8 reg_e128_8 (int x, e128_8 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e128_16:
** (
**	mov	x1, x3
**	mov	x0, x2
** |
**	mov	x0, x2
**	mov	x1, x3
** )
**	ret
*/
e128_16 reg_e128_16 (int x, e128_16 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e128_32:
** (
**	mov	x1, x3
**	mov	x0, x2
** |
**	mov	x0, x2
**	mov	x1, x3
** )
**	ret
*/
e128_32 reg_e128_32 (int x, e128_32 y) { return y; } /* { dg-note {parameter passing for argument of type 'e128_32' changed in GCC 14.1} } */

/*
** reg_e128:
** (
**	mov	x1, x3
**	mov	x0, x2
** |
**	mov	x0, x2
**	mov	x1, x3
** )
**	ret
*/
e128 reg_e128 (int x, e128 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e64_4:
**	mov	x0, x1
**	ret
*/
e64_4 reg_e64_4 (int x, e64_4 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e64_8:
**	mov	x0, x1
**	ret
*/
e64_8 reg_e64_8 (int x, e64_8 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e64_16:
**	mov	x0, x1
**	ret
*/
e64_16 reg_e64_16 (int x, e64_16 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e64_32:
**	mov	x0, x1
**	ret
*/
e64_32 reg_e64_32 (int x, e64_32 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** reg_e64:
**	mov	x0, x1
**	ret
*/
e64 reg_e64 (int x, e64 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e128_4:
**	ldp	x0, x1, \[sp, #?16\]
**	ret
*/
e128_4 stack_e128_4 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_4 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e128_8:
**	ldp	x0, x1, \[sp, #?16\]
**	ret
*/
e128_8 stack_e128_8 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_8 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e128_16:
**	ldp	x0, x1, \[sp, #?16\]
**	ret
*/
e128_16 stack_e128_16 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_16 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e128_32:
**	ldp	x0, x1, \[sp, #?16\]
**	ret
*/
e128_32 stack_e128_32 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128_32 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e128:
**	ldp	x0, x1, \[sp, #?16\]
**	ret
*/
e128 stack_e128 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e128 y) { return y; } /* { dg-bogus {parameter passing} } */

/*
** stack_e64_4:
**	ldr	x0, \[sp, #?8\]
**	ret
*/
e64_4 stack_e64_4 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_4 y) { return y; } /* { dg-note {parameter passing for argument of type 'e64_4' changed in GCC 14.1} } */

/*
** stack_e64_8:
**	ldr	x0, \[sp, #?8\]
**	ret
*/
e64_8 stack_e64_8 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_8 y) { return y; } /* { dg-note {parameter passing for argument of type 'e64_8' changed in GCC 14.1} } */

/*
** stack_e64_16:
**	ldr	x0, \[sp, #?8\]
**	ret
*/
e64_16 stack_e64_16 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_16 y) { return y; } /* { dg-note {parameter passing for argument of type 'e64_16' changed in GCC 14.1} } */

/*
** stack_e64_32:
**	ldr	x0, \[sp, #?8\]
**	ret
*/
e64_32 stack_e64_32 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64_32 y) { return y; } /* { dg-note {parameter passing for argument of type 'e64_32' changed in GCC 14.1} } */

/*
** stack_e64:
**	ldr	x0, \[sp, #?8\]
**	ret
*/
e64 stack_e64 (u128 x0, u128 x2, u128 x4, u128 x6, int x, e64 y) { return y; } /* { dg-note {parameter passing for argument of type 'e64' changed in GCC 14.1} } */

void callee (int n, ...);

void
caller ()
{
  callee (1, e128_4::A); /* { dg-bogus {parameter passing} } */
  callee (1, e128_8::A); /* { dg-bogus {parameter passing} } */
  callee (1, e128_16::A); /* { dg-bogus {parameter passing} } */
  callee (1, e128_32::A); /* { dg-note {parameter passing for argument of type 'e128_32' changed in GCC 14.1} } */
  callee (1, e128::A); /* { dg-bogus {parameter passing} } */

  callee (1, e64_4::A); /* { dg-bogus {parameter passing} } */
  callee (1, e64_8::A); /* { dg-bogus {parameter passing} } */
  callee (1, e64_16::A); /* { dg-bogus {parameter passing} } */
  callee (1, e64_32::A); /* { dg-bogus {parameter passing} } */
  callee (1, e64::A); /* { dg-bogus {parameter passing} } */

#define LOTS 1, 2, 3, 4, 5, 6, 7, 8, 9

  callee (LOTS, e128_4::A); /* { dg-bogus {parameter passing} } */
  callee (LOTS, e128_8::A); /* { dg-bogus {parameter passing} } */
  callee (LOTS, e128_16::A); /* { dg-bogus {parameter passing} } */
  callee (LOTS, e128_32::A); /* { dg-bogus {parameter passing} } */
  callee (LOTS, e128::A); /* { dg-bogus {parameter passing} } */

  callee (LOTS, e64_4::A); /* { dg-note {parameter passing for argument of type 'e64_4' changed in GCC 14.1} } */
  callee (LOTS, e64_8::A); /* { dg-note {parameter passing for argument of type 'e64_8' changed in GCC 14.1} } */
  callee (LOTS, e64_16::A); /* { dg-note {parameter passing for argument of type 'e64_16' changed in GCC 14.1} } */
  callee (LOTS, e64_32::A); /* { dg-note {parameter passing for argument of type 'e64_32' changed in GCC 14.1} } */
  callee (LOTS, e64::A); /* { dg-note {parameter passing for argument of type 'e64' changed in GCC 14.1} } */
}

void
va (volatile void *ptr, ...)
{
  va_list ap;
  va_start (ap, ptr);
  *(volatile e128_4 *) ptr = va_arg (ap, e128_4); /* { dg-bogus {parameter passing} } */
  *(volatile e128_8 *) ptr = va_arg (ap, e128_8); /* { dg-bogus {parameter passing} } */
  *(volatile e128_16 *) ptr = va_arg (ap, e128_16); /* { dg-bogus {parameter passing} } */
  *(volatile e128_32 *) ptr = va_arg (ap, e128_32); /* { dg-bogus {parameter passing} } */
  *(volatile e128 *) ptr = va_arg (ap, e128); /* { dg-bogus {parameter passing} } */
  *(volatile e64_4 *) ptr = va_arg (ap, e64_4); /* { dg-note {parameter passing for argument of type 'e64_4' changed in GCC 14.1} } */
  *(volatile e64_8 *) ptr = va_arg (ap, e64_8); /* { dg-note {parameter passing for argument of type 'e64_8' changed in GCC 14.1} } */
  *(volatile e64_16 *) ptr = va_arg (ap, e64_16); /* { dg-note {parameter passing for argument of type 'e64_16' changed in GCC 14.1} } */
  *(volatile e64_32 *) ptr = va_arg (ap, e64_32); /* { dg-note {parameter passing for argument of type 'e64_32' changed in GCC 14.1} } */
  *(volatile e64 *) ptr = va_arg (ap, e64); /* { dg-note {parameter passing for argument of type 'e64' changed in GCC 14.1} } */
}

}
