/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svint32_t s32)
{
  svpmov_lane (s32, 0); /* { dg-error {ACLE function 'svpmov_lane_s32' requires ISA extension 'sve2p1'} } */
}

#pragma GCC target "+sve2p1"

void
f2 (svbool_t pg, svint8_t s8, svuint16_t u16, svint32_t s32, svuint32_t u32,
    svfloat32_t f32, svuint64_t u64, svuint32x2_t u32x2, int x)
{
  svpmov_lane (s8); /* { dg-error {too few arguments to function 'svpmov_lane'} } */
  svpmov_lane (s8, 0, 0); /* { dg-error {too many arguments to function 'svpmov_lane'} } */

  svpmov_lane (0, 0); /* { dg-error {passing 'int' to argument 1 of 'svpmov_lane', which expects an SVE type rather than a scalar type} } */
  svpmov_lane (pg, 0); /* { dg-error {'svpmov_lane' has no form that takes 'svbool_t' arguments} } */
  svpmov_lane (s32, 0);
  svpmov_lane (u32, 0);
  svpmov_lane (f32, 0); /* { dg-error {'svpmov_lane' has no form that takes 'svfloat32_t' arguments} } */
  svpmov_lane (u32x2, 0); /* { dg-error {passing 'svuint32x2_t' to argument 1 of 'svpmov_lane', which expects a single SVE vector rather than a tuple} } */

  svpmov_lane (s8, s8); /* { dg-error {argument 2 of 'svpmov_lane' must be an integer constant expression} } */
  svpmov_lane (s8, pg); /* { dg-error {argument 2 of 'svpmov_lane' must be an integer constant expression} } */
  svpmov_lane (s8, x); /* { dg-error {argument 2 of 'svpmov_lane' must be an integer constant expression} } */
  svpmov_lane (s8, -1); /* { dg-error {passing -1 to argument 2 of 'svpmov_lane', which expects the value 0} } */
  svpmov_lane (s8, 1); /* { dg-error {passing 1 to argument 2 of 'svpmov_lane', which expects the value 0} } */

  svpmov_lane (u16, -1); /* { dg-error {passing -1 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 1\]} } */
  svpmov_lane (u16, 2); /* { dg-error {passing 2 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 1\]} } */

  svpmov_lane (s32, -1); /* { dg-error {passing -1 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 3\]} } */
  svpmov_lane (s32, 4); /* { dg-error {passing 4 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 3\]} } */

  svpmov_lane (u64, -1); /* { dg-error {passing -1 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 7\]} } */
  svpmov_lane (u64, 8); /* { dg-error {passing 8 to argument 2 of 'svpmov_lane', which expects a value in the range \[0, 7\]} } */
}
