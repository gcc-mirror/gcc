/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

void
f1 (svbool_t pg, svint8_t s8, svfloat16_t f16, svfloat32_t f32,
    svfloat64_t f64, svint32_t s32, int i)
{
  svdup_laneq (f32); /* { dg-error {too few arguments to function 'svdup_laneq'} } */
  svdup_laneq (f32, 0, 0); /* { dg-error {too many arguments to function 'svdup_laneq'} } */
  svdup_laneq (pg, 0); /* { dg-error {'svdup_laneq' has no form that takes 'svbool_t' arguments} } */
  svdup_laneq (1, 0); /* { dg-error {passing 'int' to argument 1 of 'svdup_laneq', which expects an SVE type rather than a scalar} } */
  svdup_laneq (f32, s32); /* { dg-error {argument 2 of 'svdup_laneq' must be an integer constant expression} } */
  svdup_laneq (f32, i); /* { dg-error {argument 2 of 'svdup_laneq' must be an integer constant expression} } */

  svdup_laneq (s8, 0);
  svdup_laneq (s8, 15);
  svdup_laneq (s8, 16); /* { dg-error {passing 16 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 15\]} } */
  svdup_laneq (s8, -1); /* { dg-error {passing -1 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 15\]} } */

  svdup_laneq (f16, 0);
  svdup_laneq (f16, 7);
  svdup_laneq (f16, 8); /* { dg-error {passing 8 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 7\]} } */
  svdup_laneq (f16, -1); /* { dg-error {passing -1 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 7\]} } */

  svdup_laneq (f32, 0);
  svdup_laneq (f32, 3);
  svdup_laneq (f32, 4); /* { dg-error {passing 4 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 3\]} } */
  svdup_laneq (f32, -1); /* { dg-error {passing -1 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 3\]} } */

  svdup_laneq (s32, 0);
  svdup_laneq (s32, 3);
  svdup_laneq (s32, 4); /* { dg-error {passing 4 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 3\]} } */
  svdup_laneq (s32, -1); /* { dg-error {passing -1 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 3\]} } */

  svdup_laneq (f64, 0);
  svdup_laneq (f64, 1);
  svdup_laneq (f64, 2); /* { dg-error {passing 2 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 1\]} } */
  svdup_laneq (f64, -1); /* { dg-error {passing -1 to argument 2 of 'svdup_laneq', which expects a value in the range \[0, 1\]} } */
}
