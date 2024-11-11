/* Test the fp8 ACLE intrinsics family.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9.4-a+fp8" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
**stacktest1:
**	umov	w0, v0.b\[0\]
**	sub	sp, sp, #16
**	strb	w0, \[sp, 15\]
**	ldr	b0, \[sp, 15\]
**	add	sp, sp, 16
**	ret
*/
mfloat8_t
stacktest1 (mfloat8_t __a)
{
  volatile mfloat8_t b = __a;
  return b;
}

/*
**fp8_mov_ww:
**	dup	b1, v2.b\[0\]
**	ret
*/
void
fp8_mov_ww (void)
{
  register mfloat8_t x asm ("h2");
  register mfloat8_t y asm ("h1");
  asm volatile ("" : "=w"(x));
  y = x;
  asm volatile ("" ::"w"(y));
}

/*
**fp8_mov_rw:
**	dup	v1.8b, w1
**	ret
*/
void
fp8_mov_rw (void)
{
  register mfloat8_t x asm ("w1");
  register mfloat8_t y asm ("h1");
  asm volatile ("" : "=r"(x));
  y = x;
  asm volatile ("" ::"w"(y));
}

/*
**fp8_mov_wr:
**	umov	w1, v1.b\[0\]
**	ret
*/
void
fp8_mov_wr (void)
{
  register mfloat8_t x asm ("h1");
  register mfloat8_t y asm ("w1");
  asm volatile ("" : "=w"(x));
  y = x;
  asm volatile ("" ::"r"(y));
}

/*
**fp8_mov_rr:
**	mov	w1, w2
**	ret
*/
void
fp8_mov_rr (void)
{
  register mfloat8_t x asm ("w2");
  register mfloat8_t y asm ("w1");
  asm volatile ("" : "=r"(x));
  y = x;
  asm volatile ("" ::"r"(y));
}

/*
**fp8_mov_rm:
**	strb	w2, \[x0\]
**	ret
*/
void
fp8_mov_rm (mfloat8_t *ptr)
{
  register mfloat8_t x asm ("w2");
  asm volatile ("" : "=r"(x));
  *ptr = x;
}

/*
**fp8_mov_mr:
**	ldrb	w2, \[x0\]
**	ret
*/
void
fp8_mov_mr (mfloat8_t *ptr)
{
  register mfloat8_t y asm ("w2");
  y = *ptr;
  asm volatile ("" ::"r"(y));
}

/*
**fp8_str_r:
**	str	b2, \[x0\]
**	ret
*/
void
fp8_str_r (mfloat8_t *ptr)
{
  register mfloat8_t x asm ("v2");
  asm volatile ("" : "=w"(x));
  *ptr = x;
}

/*
**fp8_ldr_r:
**	ldr	b2, \[x0\]
**	ret
*/
void
fp8_ldr_r (mfloat8_t *ptr)
{
  register mfloat8_t y asm ("v2");
  y = *ptr;
  asm volatile ("" ::"w"(y));
}
