/* { dg-options "-O2 -fpack-struct -mstrict-align -fno-stack-protector" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

extern "C" {

/*
** ld2:
**	...
**	ld2	.*
**	...
** (
**	strb	.*
** |
**	bl	memcpy
** )
**	...
*/
void
ld2 (int32x4x2_t *a, int32_t *b)
{
  *a = vld2q_s32 (b);
}

/*
** ld3:
**	...
**	ld3	.*
**	...
** (
**	strb	.*
** |
**	bl	memcpy
** )
**	...
*/
void
ld3 (int32x4x3_t *a, int32_t *b)
{
  *a = vld3q_s32 (b);
}

/*
** ld4:
**	...
**	ld4	.*
**	...
** (
**	strb	.*
** |
**	bl	memcpy
** )
**	...
*/
void
ld4 (int32x4x4_t *a, int32_t *b)
{
  *a = vld4q_s32 (b);
}

/*
** ret:
**	...
**	ldp	q0, q1, \[x0\]
**	ldr	q2, \[x0, #?32\]
**	...
*/
int32x4x3_t
ret (int32x4_t *ptr)
{
  return (int32x4x3_t) { ptr[0], ptr[1], ptr[2] };
}

/*
** arg:
**	...
**	stp	d0, d1, \[x0\]
**	...
*/
void
arg (int32x2x2_t arg, int32x2_t *ptr)
{
  ptr[0] = arg.val[0];
  ptr[1] = arg.val[1];
}

}
