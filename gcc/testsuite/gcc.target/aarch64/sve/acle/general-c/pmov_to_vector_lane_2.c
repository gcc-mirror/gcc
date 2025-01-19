/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

void
f1 (svbool_t pg, svint16_t s16, svuint32_t u32, svint64_t s64, int x)
{
  svpmov_lane_s16_m (s16, pg, x); /* { dg-error {argument 3 of 'svpmov_lane_s16_m' must be an integer constant expression} } */
  svpmov_lane_s16_m (s16, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_s16_m', which expects the value 1} } */
  svpmov_lane_s16_m (s16, pg, 2); /* { dg-error {passing 2 to argument 3 of 'svpmov_lane_s16_m', which expects the value 1} } */

  svpmov_lane_u32_m (u32, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_u32_m', which expects a value in the range \[1, 3\]} } */
  svpmov_lane_u32_m (u32, pg, 4); /* { dg-error {passing 4 to argument 3 of 'svpmov_lane_u32_m', which expects a value in the range \[1, 3\]} } */

  svpmov_lane_s64_m (s64, pg, 0); /* { dg-error {passing 0 to argument 3 of 'svpmov_lane_s64_m', which expects a value in the range \[1, 7\]} } */
  svpmov_lane_s64_m (s64, pg, 8); /* { dg-error {passing 8 to argument 3 of 'svpmov_lane_s64_m', which expects a value in the range \[1, 7\]} } */
}
