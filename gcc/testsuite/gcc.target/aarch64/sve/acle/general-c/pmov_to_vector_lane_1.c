/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svint32_t s32, svbool_t pg)
{
  svpmov_lane_m (s32, pg, 1); /* { dg-error {ACLE function 'svpmov_lane_s32_m' requires ISA extension 'sve2p1'} } */
}

#pragma GCC target "+sve2p1"

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint16_t u16, svint32_t s32,
    svuint32_t u32, svfloat32_t f32, svuint64_t u64, svuint32x2_t u32x2, int x)
{
  svpmov_lane_m (u16, pg); /* { dg-error {too few arguments to function 'svpmov_lane_m'} } */
  svpmov_lane_m (u16, pg, 1, 0); /* { dg-error {too many arguments to function 'svpmov_lane_m'} } */

  svpmov_lane_m (0, pg, 1); /* { dg-error {passing 'int' to argument 1 of 'svpmov_lane_m', which expects an SVE type rather than a scalar type} } */
  svpmov_lane_m (pg, pg, 1); /* { dg-error {'svpmov_lane_m' has no form that takes 'svbool_t' arguments} } */
  svpmov_lane_m (s32, pg, 1);
  svpmov_lane_m (u32, pg, 1);
  svpmov_lane_m (f32, pg, 1); /* { dg-error {'svpmov_lane_m' has no form that takes 'svfloat32_t' arguments} } */
  svpmov_lane_m (u32x2, pg, 1); /* { dg-error {passing 'svuint32x2_t' to argument 1 of 'svpmov_lane_m', which expects a single SVE vector rather than a tuple} } */

  svpmov_lane_m (u16, u16, 1); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svpmov_lane_m', which expects 'svbool_t'} } */
  svpmov_lane_m (u16, x, 1); /* { dg-error {passing 'int' to argument 2 of 'svpmov_lane_m', which expects 'svbool_t'} } */
  svpmov_lane_m (u16, u32x2, 1); /* { dg-error {passing 'svuint32x2_t' to argument 2 of 'svpmov_lane_m', which expects 'svbool_t'} } */

  svpmov_lane_m (s8, pg, 0); /* { dg-error {'svpmov_lane_m' has no form that takes 'svint8_t' arguments} } */
  svpmov_lane_m (u8, pg, 0); /* { dg-error {'svpmov_lane_m' has no form that takes 'svuint8_t' arguments} } */

  svpmov_lane_m (u16, pg, u16); /* { dg-error {argument 3 of 'svpmov_lane_m' must be an integer constant expression} } */
  svpmov_lane_m (u16, pg, pg); /* { dg-error {argument 3 of 'svpmov_lane_m' must be an integer constant expression} } */
  svpmov_lane_m (u16, pg, x); /* { dg-error {argument 3 of 'svpmov_lane_m' must be an integer constant expression} } */
  svpmov_lane_m (u16, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_m', which expects the value 1} } */
  svpmov_lane_m (u16, pg, 2); /* { dg-error {passing 2 to argument 3 of 'svpmov_lane_m', which expects the value 1} } */

  svpmov_lane_m (s32, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_m', which expects a value in the range \[1, 3\]} } */
  svpmov_lane_m (s32, pg, 4); /* { dg-error {passing 4 to argument 3 of 'svpmov_lane_m', which expects a value in the range \[1, 3\]} } */

  svpmov_lane_m (u64, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_m', which expects a value in the range \[1, 7\]} } */
  svpmov_lane_m (u64, pg, 8); /* { dg-error {passing 8 to argument 3 of 'svpmov_lane_m', which expects a value in the range \[1, 7\]} } */
}
