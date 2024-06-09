/* Test the __arm_[r,w]sr ACLE intrinsics family.  */
/* Check that function variants for different data types handle types correctly.  */
/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8-a" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

#pragma GCC push_options
#pragma GCC target ("arch=armv9.4-a+d128")

#ifndef __ARM_FEATURE_SYSREG128
#error "__ARM_FEATURE_SYSREG128 feature macro not defined."
#endif

/*
** get_rsr128:
**	mrrs	x0, x1, s3_0_c7_c4_0
** ...
*/
__uint128_t
get_rsr128 ()
{
  __arm_rsr128 ("par_el1");
}

/*
** set_wsr128:
** ...
** 	msrr	s3_0_c7_c4_0, x0, x1
** ...
*/
void
set_wsr128 (__uint128_t c)
{
  __arm_wsr128 ("par_el1", c);
}

#pragma GCC pop_options

/*
** get_rsr:
** ...
** 	mrs	x([0-9]+), s2_1_c0_c7_4
** 	add	w\1, w\1, 1
** ...
*/
int
get_rsr ()
{
  int a =  __arm_rsr ("trcseqstr");
  return a+1;
}

/*
** get_rsrf:
** 	mrs	x([0-9]+), s2_1_c0_c7_4
** 	fmov	s[0-9]+, w\1
** ...
*/
float
get_rsrf ()
{
  return __arm_rsrf ("trcseqstr");
}

/*
** get_rsrp:
** 	mrs	x0, s2_1_c0_c7_4
** 	ret
*/
void *
get_rsrp ()
{
  return __arm_rsrp ("trcseqstr");
}

/*
** get_rsr64:
** 	mrs	x0, s2_1_c0_c7_4
** 	ret
*/
long long
get_rsr64 ()
{
  return __arm_rsr64 ("trcseqstr");
}

/*
** get_rsrf64:
** 	mrs	x([0-9]+), s2_1_c0_c7_4
** 	fmov	d[0-9]+, x\1
** ...
*/
double
get_rsrf64 ()
{
  return __arm_rsrf64 ("trcseqstr");
}

/*
** set_wsr32:
** ...
** 	add	w([0-9]+), w\1, 1
** 	msr	s2_1_c0_c7_4, x\1
** ...
*/
void
set_wsr32 (int a)
{
  __arm_wsr ("trcseqstr", a+1);
}

/*
** set_wsrp:
** ...
** 	msr	s2_1_c0_c7_4, x[0-9]+
** ...
*/
void
set_wsrp (void *a)
{
  __arm_wsrp ("trcseqstr", a);
}

/*
** set_wsr64:
** ...
** 	msr	s2_1_c0_c7_4, x[0-9]+
** ...
*/
void
set_wsr64 (long long a)
{
  __arm_wsr64 ("trcseqstr", a);
}

/*
** set_wsrf32:
** ...
** 	fmov	w([0-9]+), s[0-9]+
** 	msr	s2_1_c0_c7_4, x\1
** ...
*/
void
set_wsrf32 (float a)
{
  __arm_wsrf ("trcseqstr", a);
}

/*
** set_wsrf64:
** ...
** 	fmov	x([0-9]+), d[0-9]+
** 	msr	s2_1_c0_c7_4, x\1
** ...
*/
void
set_wsrf64 (double a)
{
  __arm_wsrf64 ("trcseqstr", a);
}

/*
** set_custom:
** ...
** 	mrs	x0, s1_2_c3_c4_5
** ...
** 	msr	s1_2_c3_c4_5, x0
** ...
*/
void set_custom ()
{
  __uint64_t b = __arm_rsr64 ("S1_2_C3_C4_5");
  __arm_wsr64 ("S1_2_C3_C4_5", b);
}
