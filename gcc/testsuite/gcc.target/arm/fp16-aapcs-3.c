/* { dg-do compile }  */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_fp16_alternative } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test __fp16 arguments and return value in registers (hard-float).  */

void
swap (__fp16, __fp16);

/*
** F:
** ...
** (

Below block is for non-armv8.1
** (
**	vmov\.f32	(s[3-9]|s1[0-5]), s0
** ...
**	vmov\.f32	s0, s1
** ...
**	vmov\.f32	s1, \1
** |
**	vmov\.f32	(s[3-9]|s1[0-5]), s1
** ...
**	vmov\.f32	s1, s0
** ...
**	vmov\.f32	s0, \2
** )
**	vstr\.32	s2, \[sp, #4\]	@ int
**	bl	swap
**	vldr\.32	s2, \[sp, #4\]	@ int
**	vmov\.f32	s0, s2

** |

Below block is for armv8.1
** (
**	vmov	(s[3-9]|s1[0-5]), s0
** ...
**	vmov	s0, s1
** ...
**	vmov	s1, \3
** |
**	vmov	(s[3-9]|s1[0-5]), s1
** ...
**	vmov	s1, s0
** ...
**	vmov	s0, \4
** )
**	vstr\.32	s2, \[sp, #4\]	@ int
**	bl	swap
**	vldr\.16	s0, \[sp, #4\]

** )
** ...
*/
__fp16
F (__fp16 a, __fp16 b, __fp16 c)
{
  swap (b, a);
  return c;
}
