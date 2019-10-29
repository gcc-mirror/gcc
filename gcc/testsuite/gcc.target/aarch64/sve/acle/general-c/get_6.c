/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svint32_t s32, svint32x4_t s32x4, svfloat32x4_t f32x4,
    svint32x2_t s32x2, int x)
{
  const int one = 1;
  svfloat64_t f64;

  s32 = svget4_s32 (s32x4); /* { dg-error {too few arguments to function 'svget4_s32'} } */
  s32 = svget4_s32 (s32x4, 1, 2); /* { dg-error {too many arguments to function 'svget4_s32'} } */
  s32 = svget4_s32 (s32, 0); /* { dg-error {incompatible type for argument 1 of 'svget4_s32'} } */
  s32 = svget4_s32 (f32x4, 0); /* { dg-error {incompatible type for argument 1 of 'svget4_s32'} } */
  s32 = svget4_s32 (s32x2, 0); /* { dg-error {incompatible type for argument 1 of 'svget4_s32'} } */
  s32 = svget4_s32 (pg, 0); /* { dg-error {incompatible type for argument 1 of 'svget4_s32'} } */
  s32 = svget4_s32 (s32x4, x); /* { dg-error {argument 2 of 'svget4_s32' must be an integer constant expression} } */
  s32 = svget4_s32 (s32x4, 0);
  f64 = svget4_s32 (s32x4, 0); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svint32_t'} } */
  s32 = svget4_s32 (s32x4, 1);
  s32 = svget4_s32 (s32x4, 2);
  s32 = svget4_s32 (s32x4, 3);
  s32 = svget4_s32 (s32x4, 4); /* { dg-error {passing 4 to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, 5); /* { dg-error {passing 5 to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, ~0U); /* { dg-error {passing [^ ]* to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, one); /* { dg-error {argument 2 of 'svget4_s32' must be an integer constant expression} } */
  s32 = svget4_s32 (s32x4, 3 - 2);
  s32 = svget4_s32 (s32x4, 1.0);

  return f64;
}
