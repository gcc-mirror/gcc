/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

svfloat64_t
f1 (svbool_t pg, svuint8_t u8, svuint8x2_t u8x2, svint8x2_t s8x2,
    svuint8x3_t u8x3, int x)
{
  const int one = 1;
  svfloat64_t f64;

  u8 = svget2_u8 (u8x2); /* { dg-error {too few arguments to function 'svget2_u8'} } */
  u8 = svget2_u8 (u8x2, 1, 2); /* { dg-error {too many arguments to function 'svget2_u8'} } */
  u8 = svget2_u8 (u8, 0); /* { dg-error {incompatible type for argument 1 of 'svget2_u8'} } */
  u8 = svget2_u8 (s8x2, 0); /* { dg-error {incompatible type for argument 1 of 'svget2_u8'} } */
  u8 = svget2_u8 (u8x3, 0); /* { dg-error {incompatible type for argument 1 of 'svget2_u8'} } */
  u8 = svget2_u8 (pg, 0); /* { dg-error {incompatible type for argument 1 of 'svget2_u8'} } */
  u8 = svget2_u8 (u8x2, x); /* { dg-error {argument 2 of 'svget2_u8' must be an integer constant expression} } */
  u8 = svget2_u8 (u8x2, 0);
  f64 = svget2_u8 (u8x2, 0); /* { dg-error {incompatible types when assigning to type 'svfloat64_t' from type 'svuint8_t'} } */
  u8 = svget2_u8 (u8x2, 1);
  u8 = svget2_u8 (u8x2, 2); /* { dg-error {passing 2 to argument 2 of 'svget2_u8', which expects a value in the range \[0, 1\]} } */
  u8 = svget2_u8 (u8x2, 3); /* { dg-error {passing 3 to argument 2 of 'svget2_u8', which expects a value in the range \[0, 1\]} } */
  u8 = svget2_u8 (u8x2, 4); /* { dg-error {passing 4 to argument 2 of 'svget2_u8', which expects a value in the range \[0, 1\]} } */
  u8 = svget2_u8 (u8x2, 5); /* { dg-error {passing 5 to argument 2 of 'svget2_u8', which expects a value in the range \[0, 1\]} } */
  u8 = svget2_u8 (u8x2, ~0U); /* { dg-error {passing [^ ]* to argument 2 of 'svget2_u8', which expects a value in the range \[0, 1\]} } */
  u8 = svget2_u8 (u8x2, one); /* { dg-error {argument 2 of 'svget2_u8' must be an integer constant expression} } */
  u8 = svget2_u8 (u8x2, 3 - 2);
  u8 = svget2_u8 (u8x2, 1.0);

  return f64;
}
