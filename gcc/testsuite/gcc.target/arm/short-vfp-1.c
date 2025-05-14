/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_fp_hard_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v7a_fp_hard } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** test_sisf:
** 	vcvt.s32.f32	(s[0-9]+), s0
** 	vmov	r0, \1	@ int
** 	bx	lr
*/
int
test_sisf (float x)
{
  return (int)x;
}

/*
** test_hisf:
** 	vcvt.s32.f32	(s[0-9]+), s0
** 	vmov	(r[0-9]+), \1	@ int
** 	sxth	r0, \2
** 	bx	lr
*/
short
test_hisf (float x)
{
  return (short)x;
}

/*
** test_sfsi:
** 	vmov	(s[0-9]+), r0	@ int
** 	vcvt.f32.s32	s0, \1
** 	bx	lr
*/
float
test_sfsi (int x)
{
  return (float)x;
}

/*
** test_sfhi:
** 	vmov	(s[0-9]+), r0	@ int
** 	vcvt.f32.s32	s0, \1
** 	bx	lr
*/
float
test_sfhi (short x)
{
  return (float)x;
}

/*
** test_hisi:
** 	sxth	r0, r0
** 	bx	lr
*/
short
test_hisi (int x)
{
  return (short)x;
}

/*
** test_sihi:
** 	bx	lr
*/
int
test_sihi (short x)
{
  return (int)x;
}
