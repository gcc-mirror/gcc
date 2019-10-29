/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8, int i, float f)
{
  svdupq_lane (u8); /* { dg-error {too few arguments to function 'svdupq_lane'} } */
  svdupq_lane (u8, 0, 0); /* { dg-error {too many arguments to function 'svdupq_lane'} } */
  svdupq_lane (0, 0); /* { dg-error {passing 'int' to argument 1 of 'svdupq_lane', which expects an SVE vector type} } */
  svdupq_lane (u8, 0);
  svdupq_lane (u8, -1);
  svdupq_lane (u8, i);
  svdupq_lane (u8, f);
  svdupq_lane (u8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svdupq_lane', which expects 'uint64_t'} } */
  svdupq_lane (pg, 0); /* { dg-error {'svdupq_lane' has no form that takes 'svbool_t' arguments} } */
}
