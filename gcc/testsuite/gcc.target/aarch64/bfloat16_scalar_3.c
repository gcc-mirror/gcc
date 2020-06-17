/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-additional-options "-march=armv8.2-a -O3 --save-temps -std=gnu90" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_bf16.h>

/*
**stacktest1:
**	sub	sp, sp, #16
**	str	h0, \[sp, 14\]
**	ldr	h0, \[sp, 14\]
**	add	sp, sp, 16
**	ret
*/
bfloat16_t stacktest1 (bfloat16_t __a)
{
  volatile bfloat16_t b = __a;
  return b;
}

/*
**bfloat_mov_ww:
**	mov	v1.h\[0\], v2.h\[0\]
**	ret
*/
void bfloat_mov_ww (void)
{
  register bfloat16_t x asm ("h2");
  register bfloat16_t y asm ("h1");
  asm volatile ("" : "=w" (x));
  y = x;
  asm volatile ("" :: "w" (y));
}

/*
**bfloat_mov_rw:
**	dup	v1.4h, w1
**	ret
*/
void bfloat_mov_rw (void)
{
  register bfloat16_t x asm ("w1");
  register bfloat16_t y asm ("h1");
  asm volatile ("" : "=r" (x));
  y = x;
  asm volatile ("" :: "w" (y));
}

/*
**bfloat_mov_wr:
**	umov	w1, v1.h\[0\]
**	ret
*/
void bfloat_mov_wr (void)
{
  register bfloat16_t x asm ("h1");
  register bfloat16_t y asm ("w1");
  asm volatile ("" : "=w" (x));
  y = x;
  asm volatile ("" :: "r" (y));
}

/*
**bfloat_mov_rr:
**	mov	w1, w2
**	ret
*/
void bfloat_mov_rr (void)
{
  register bfloat16_t x asm ("w2");
  register bfloat16_t y asm ("w1");
  asm volatile ("" : "=r" (x));
  y = x;
  asm volatile ("" :: "r" (y));
}

/*
**bfloat_mov_rm:
**	strh	w2, \[x0\]
**	ret
*/
void bfloat_mov_rm (bfloat16_t *ptr)
{
   register bfloat16_t x asm ("w2");
   asm volatile ("" : "=r" (x));
   *ptr = x;
}

/*
**bfloat_mov_mr:
**	ldrh	w2, \[x0\]
**	ret
*/
void bfloat_mov_mr (bfloat16_t *ptr)
{
   register bfloat16_t y asm ("w2");
   y = *ptr;
   asm volatile ("" :: "r" (y));
}

