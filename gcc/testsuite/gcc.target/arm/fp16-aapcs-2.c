/* { dg-do compile }  */
/* { dg-require-effective-target arm_fp16_ok } */
/* { dg-options "-mfloat-abi=softfp -O2 -mno-long-calls" }  */
/* { dg-add-options arm_fp16_ieee } */
/* { dg-skip-if "incompatible float-abi" { arm*-*-* } { "-mfloat-abi=hard" } } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test __fp16 arguments and return value in registers (softfp).  */

void
swap (__fp16, __fp16);

/*
** F:
** ...
** (
**	mov	r3, r0	@ __fp16
** ...
**	mov	r0, r1	@ __fp16
** ...
**	mov	r1, r3	@ __fp16
** |
**	mov	r3, r1	@ __fp16
** ...
**	mov	r1, r0	@ __fp16
** ...
**	mov	r0, r3	@ __fp16
** )
** ...
*/
/*
** F: { target arm_little_endian }
** ...
**	str	r2, \[sp, #4\]
**	bl	swap
**	ldrh	r0, \[sp, #4\]	@ __fp16
** ...
*/
__fp16
F (__fp16 a, __fp16 b, __fp16 c)
{
  swap (b, a);
  return c;
}
