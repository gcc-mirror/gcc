/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svuint8_t u8, svuint8x2_t u8x2, svuint8x3_t u8x3, int x)
{
  const int one = 1;
  svfloat64_t f64;

  u8x2 = svset2 (u8x2); /* { dg-error {too few arguments to function 'svset2'} } */
  u8x2 = svset2 (u8x2, 1); /* { dg-error {too few arguments to function 'svset2'} } */
  u8x2 = svset2 (u8x2, 1, u8, 3); /* { dg-error {too many arguments to function 'svset2'} } */
  u8x2 = svset2 (u8, 0, u8); /* { dg-error {passing single vector 'svuint8_t' to argument 1 of 'svset2', which expects a tuple of 2 vectors} } */
  u8x2 = svset2 (u8x3, 0, u8); /* { dg-error {passing 'svuint8x3_t' to argument 1 of 'svset2', which expects a tuple of 2 vectors} } */
  u8x2 = svset2 (pg, 0, u8); /* { dg-error {passing 'svbool_t' to argument 1 of 'svset2', which expects a tuple of 2 vectors} } */
  u8x2 = svset2 (u8x2, 0, u8x2); /* { dg-error {passing 'svuint8x2_t' to argument 3 of 'svset2', which expects a single SVE vector rather than a tuple} } */
  u8x2 = svset2 (u8x2, 0, f64); /* { dg-error {passing 'svfloat64_t' instead of the expected 'svuint8_t' to argument 3 of 'svset2', after passing 'svuint8x2_t' to argument 1} } */
  u8x2 = svset2 (u8x2, 0, pg); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 3 of 'svset2', after passing 'svuint8x2_t' to argument 1} } */
  u8x2 = svset2 (u8x2, x, u8); /* { dg-error {argument 2 of 'svset2' must be an integer constant expression} } */
  u8x2 = svset2 (u8x2, 0, u8);
  f64 = svset2 (u8x2, 0, u8); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svuint8x2_t'} } */
  u8x2 = svset2 (u8x2, 1, u8);
  u8x2 = svset2 (u8x2, 2, u8); /* { dg-error {passing 2 to argument 2 of 'svset2', which expects a value in the range \[0, 1\]} } */
  u8x2 = svset2 (u8x2, 3, u8); /* { dg-error {passing 3 to argument 2 of 'svset2', which expects a value in the range \[0, 1\]} } */
  u8x2 = svset2 (u8x2, 4, u8); /* { dg-error {passing 4 to argument 2 of 'svset2', which expects a value in the range \[0, 1\]} } */
  u8x2 = svset2 (u8x2, 5, u8); /* { dg-error {passing 5 to argument 2 of 'svset2', which expects a value in the range \[0, 1\]} } */
  u8x2 = svset2 (u8x2, ~0U, u8); /* { dg-error {passing [^ ]* to argument 2 of 'svset2', which expects a value in the range \[0, 1\]} } */
  u8x2 = svset2 (u8x2, one, u8); /* { dg-error {argument 2 of 'svset2' must be an integer constant expression} } */
  u8x2 = svset2 (u8x2, 3 - 2, u8);
  u8x2 = svset2 (u8x2, 1.0, u8);

  return f64;
}
