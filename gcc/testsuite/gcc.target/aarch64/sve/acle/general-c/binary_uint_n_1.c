/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8, int i, float f)
{
  svdup_lane (u8); /* { dg-error {too few arguments to function 'svdup_lane'} } */
  svdup_lane (u8, 0, 0); /* { dg-error {too many arguments to function 'svdup_lane'} } */
  svdup_lane (0, 0); /* { dg-error {passing 'int' to argument 1 of 'svdup_lane', which expects an SVE type rather than a scalar} } */
  svdup_lane (u8, 0);
  svdup_lane (u8, -1);
  svdup_lane (u8, i);
  svdup_lane (u8, f);
  svdup_lane (u8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svdup_lane', which expects a scalar integer} } */
  svdup_lane (pg, 0); /* { dg-error {'svdup_lane' has no form that takes 'svbool_t' arguments} } */
}
