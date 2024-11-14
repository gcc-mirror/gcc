/* Test the __gsc* ACLE intrinsics.  */
/* { dg-do compile } */
/* { dg-options "-O1 -march=armv8.9-a -mbranch-protection=none" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
**test_gcspr:
**	mrs	x0, s3_3_c2_c5_1 // gcspr_el0
**	ret
*/
void *
test_gcspr ()
{
  return __gcspr ();
}

/*
**test_gcspopm:
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #1 // gcspopm
**	ret
*/
uint64_t
test_gcspopm ()
{
  return __gcspopm ();
}

/*
**test_gcsss:
**	sys	#3, c7, c7, #2, x0 // gcsss1
**	mov	x0, 0
**	sysl	x0, #3, c7, c7, #3 // gcsss2
**	ret
*/
void *
test_gcsss (void *stack)
{
  return __gcsss (stack);
}
