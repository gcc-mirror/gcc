/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svfloat16_t f16, svfloat16x3_t f16x3, svfloat32x3_t f32x3,
    svfloat16x4_t f16x4, int x)
{
  const int one = 1;
  svfloat64_t f64;

  f16 = svget3_f16 (f16x3); /* { dg-error {too few arguments to function 'svget3_f16'} } */
  f16 = svget3_f16 (f16x3, 1, 2); /* { dg-error {too many arguments to function 'svget3_f16'} } */
  f16 = svget3_f16 (f16, 0); /* { dg-error {incompatible type for argument 1 of 'svget3_f16'} } */
  f16 = svget3_f16 (f32x3, 0); /* { dg-error {incompatible type for argument 1 of 'svget3_f16'} } */
  f16 = svget3_f16 (f16x4, 0); /* { dg-error {incompatible type for argument 1 of 'svget3_f16'} } */
  f16 = svget3_f16 (pg, 0); /* { dg-error {incompatible type for argument 1 of 'svget3_f16'} } */
  f16 = svget3_f16 (f16x3, x); /* { dg-error {argument 2 of 'svget3_f16' must be an integer constant expression} } */
  f16 = svget3_f16 (f16x3, 0);
  f64 = svget3_f16 (f16x3, 0); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svfloat16_t'} } */
  f16 = svget3_f16 (f16x3, 1);
  f16 = svget3_f16 (f16x3, 2);
  f16 = svget3_f16 (f16x3, 3); /* { dg-error {passing 3 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, 4); /* { dg-error {passing 4 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, 5); /* { dg-error {passing 5 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, ~0U); /* { dg-error {passing [^ ]* to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, one); /* { dg-error {argument 2 of 'svget3_f16' must be an integer constant expression} } */
  f16 = svget3_f16 (f16x3, 3 - 2);
  f16 = svget3_f16 (f16x3, 1.0);

  return f64;
}
