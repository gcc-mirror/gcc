/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-additional-options "-march=armv8.2-a+bf16 -mfloat-abi=softfp -mfpu=auto" } */
/* { dg-additional-options "-O3 --save-temps -std=gnu90" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_bf16.h>

/*
**stacktest1:
**	...
**	strh	r[0-9]+, \[r[0-9]+\]	@ __bf16
**	ldrh	r[0-9]+, \[sp, #[0-9]+\]	@ __bf16
**	...
**	bx	lr
*/
bfloat16_t stacktest1 (bfloat16_t __a)
{
  volatile bfloat16_t b = __a;
  return b;
}

/*
**bfloat_mov_ww:
**	...
**	vmov.f32	s1, s15
**	...
**	bx	lr
*/
void bfloat_mov_ww (void)
{
  register bfloat16_t x asm ("s15");
  register bfloat16_t y asm ("s1");
  asm volatile ("#foo" : "=t" (x));
  y = x;
  asm volatile ("#foo" :: "t" (y));
}

/*
**bfloat_mov_rw:
**	...
**	vmov	s1, r4
**	...
**	bx	lr
*/
void bfloat_mov_rw (void)
{
  register bfloat16_t x asm ("r4");
  register bfloat16_t y asm ("s1");
  asm volatile ("#foo" : "=r" (x));
  y = x;
  asm volatile ("#foo" :: "t" (y));
}

/*
**bfloat_mov_wr:
**	...
**	vmov	r4, s1
**	...
**	bx	lr
*/
void bfloat_mov_wr (void)
{
  register bfloat16_t x asm ("s1");
  register bfloat16_t y asm ("r4");
  asm volatile ("#foo" : "=t" (x));
  y = x;
  asm volatile ("#foo" :: "r" (y));
}

/*
**bfloat_mov_rr:
**	...
**	mov	r4, r5	@ __bf16
**	...
**	bx	lr
*/
void bfloat_mov_rr (void)
{
  register bfloat16_t x asm ("r5");
  register bfloat16_t y asm ("r4");
  asm volatile ("#foo" : "=r" (x));
  y = x;
  asm volatile ("#foo" :: "r" (y));
}

/*
**bfloat_mov_rm:
**	...
**	strh	r4, \[.*\]	@ __bf16
**	...
**	bx	lr
*/
void bfloat_mov_rm (void)
{
  register bfloat16_t x asm ("r4");
  volatile bfloat16_t y;
  asm volatile ("#foo" : "=r" (x));
  y = x;
  asm volatile ("#foo" : : : "memory");
}

/*
**bfloat_mov_mr:
**	...
**	ldrh	r4, \[.*\]	@ __bf16
**	...
**	bx	lr
*/
void bfloat_mov_mr (void)
{
  volatile bfloat16_t x;
  register bfloat16_t y asm ("r4");
  asm volatile ("#foo" : : : "memory");
  y = x;
  asm volatile ("#foo" :: "r" (y));
}

