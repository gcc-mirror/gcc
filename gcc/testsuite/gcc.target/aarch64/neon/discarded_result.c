/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-fno-trapping-math" } */

#include "arm_neon_test.h"

/* Assert that discarding the result of a NEON intrinsic does not cause a crash during folding.  */

/*
** foo:
** ret
*/
void foo(float32x2_t a, float32x2_t b)
{
  vadd_f32 (a, b);
}
