/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svfloat16_t f16, svfloat16x3_t f16x3, svfloat16x4_t f16x4,
    int x)
{
  const int one = 1;
  svfloat64_t f64;

  f16x3 = svset3 (f16x3); /* { dg-error {too few arguments to function 'svset3'} } */
  f16x3 = svset3 (f16x3, 1); /* { dg-error {too few arguments to function 'svset3'} } */
  f16x3 = svset3 (f16x3, 1, f16, 3); /* { dg-error {too many arguments to function 'svset3'} } */
  f16x3 = svset3 (f16, 0, f16); /* { dg-error {passing single vector 'svfloat16_t' to argument 1 of 'svset3', which expects a tuple of 3 vectors} } */
  f16x3 = svset3 (f16x4, 0, f16); /* { dg-error {passing 'svfloat16x4_t' to argument 1 of 'svset3', which expects a tuple of 3 vectors} } */
  f16x3 = svset3 (pg, 0, f16); /* { dg-error {passing 'svbool_t' to argument 1 of 'svset3', which expects a tuple of 3 vectors} } */
  f16x3 = svset3 (f16x3, 0, f16x3); /* { dg-error {passing 'svfloat16x3_t' to argument 3 of 'svset3', which expects a single SVE vector rather than a tuple} } */
  f16x3 = svset3 (f16x3, 0, f64); /* { dg-error {passing 'svfloat64_t' instead of the expected 'svfloat16_t' to argument 3 of 'svset3', after passing 'svfloat16x3_t' to argument 1} } */
  f16x3 = svset3 (f16x3, 0, pg); /* { dg-error {passing 'svbool_t' instead of the expected 'svfloat16_t' to argument 3 of 'svset3', after passing 'svfloat16x3_t' to argument 1} } */
  f16x3 = svset3 (f16x3, x, f16); /* { dg-error {argument 2 of 'svset3' must be an integer constant expression} } */
  f16x3 = svset3 (f16x3, 0, f16);
  f64 = svset3 (f16x3, 0, f16); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svfloat16x3_t'} } */
  f16x3 = svset3 (f16x3, 1, f16);
  f16x3 = svset3 (f16x3, 2, f16);
  f16x3 = svset3 (f16x3, 3, f16); /* { dg-error {passing 3 to argument 2 of 'svset3', which expects a value in the range \[0, 2\]} } */
  f16x3 = svset3 (f16x3, 4, f16); /* { dg-error {passing 4 to argument 2 of 'svset3', which expects a value in the range \[0, 2\]} } */
  f16x3 = svset3 (f16x3, 5, f16); /* { dg-error {passing 5 to argument 2 of 'svset3', which expects a value in the range \[0, 2\]} } */
  f16x3 = svset3 (f16x3, ~0U, f16); /* { dg-error {passing [^ ]* to argument 2 of 'svset3', which expects a value in the range \[0, 2\]} } */
  f16x3 = svset3 (f16x3, one, f16); /* { dg-error {argument 2 of 'svset3' must be an integer constant expression} } */
  f16x3 = svset3 (f16x3, 3 - 2, f16);
  f16x3 = svset3 (f16x3, 1.0, f16);

  return f64;
}
