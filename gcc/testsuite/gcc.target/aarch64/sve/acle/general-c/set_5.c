/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svint32_t s32, svint32x4_t s32x4, svint32x2_t s32x2, int x)
{
  const int one = 1;
  svfloat64_t f64;

  s32x4 = svset4 (s32x4); /* { dg-error {too few arguments to function 'svset4'} } */
  s32x4 = svset4 (s32x4, 1); /* { dg-error {too few arguments to function 'svset4'} } */
  s32x4 = svset4 (s32x4, 1, s32, 3); /* { dg-error {too many arguments to function 'svset4'} } */
  s32x4 = svset4 (s32, 0, s32); /* { dg-error {passing single vector 'svint32_t' to argument 1 of 'svset4', which expects a tuple of 4 vectors} } */
  s32x4 = svset4 (s32x2, 0, s32); /* { dg-error {passing 'svint32x2_t' to argument 1 of 'svset4', which expects a tuple of 4 vectors} } */
  s32x4 = svset4 (pg, 0, s32); /* { dg-error {passing 'svbool_t' to argument 1 of 'svset4', which expects a tuple of 4 vectors} } */
  s32x4 = svset4 (s32x4, 0, s32x4); /* { dg-error {passing 'svint32x4_t' to argument 3 of 'svset4', which expects a single SVE vector rather than a tuple} } */
  s32x4 = svset4 (s32x4, 0, f64); /* { dg-error {passing 'svfloat64_t' instead of the expected 'svint32_t' to argument 3 of 'svset4', after passing 'svint32x4_t' to argument 1} } */
  s32x4 = svset4 (s32x4, 0, pg); /* { dg-error {passing 'svbool_t' instead of the expected 'svint32_t' to argument 3 of 'svset4', after passing 'svint32x4_t' to argument 1} } */
  s32x4 = svset4 (s32x4, x, s32); /* { dg-error {argument 2 of 'svset4' must be an integer constant expression} } */
  s32x4 = svset4 (s32x4, 0, s32);
  f64 = svset4 (s32x4, 0, s32); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svint32x4_t'} } */
  s32x4 = svset4 (s32x4, 1, s32);
  s32x4 = svset4 (s32x4, 2, s32);
  s32x4 = svset4 (s32x4, 3, s32);
  s32x4 = svset4 (s32x4, 4, s32); /* { dg-error {passing 4 to argument 2 of 'svset4', which expects a value in the range \[0, 3\]} } */
  s32x4 = svset4 (s32x4, 5, s32); /* { dg-error {passing 5 to argument 2 of 'svset4', which expects a value in the range \[0, 3\]} } */
  s32x4 = svset4 (s32x4, ~0U, s32); /* { dg-error {passing [^ ]* to argument 2 of 'svset4', which expects a value in the range \[0, 3\]} } */
  s32x4 = svset4 (s32x4, one, s32); /* { dg-error {argument 2 of 'svset4' must be an integer constant expression} } */
  s32x4 = svset4 (s32x4, 3 - 2, s32);
  s32x4 = svset4 (s32x4, 1.0, s32);

  return f64;
}
