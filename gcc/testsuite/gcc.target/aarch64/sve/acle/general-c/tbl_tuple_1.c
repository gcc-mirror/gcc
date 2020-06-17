/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

svfloat64_t
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint8x2_t s8x2, svuint8x2_t u8x2, svuint8x3_t u8x3)
{
  svfloat64_t f64;

  u8 = svtbl2 (u8x2); /* { dg-error {too few arguments to function 'svtbl2'} } */
  u8 = svtbl2 (u8x2); /* { dg-error {too few arguments to function 'svtbl2'} } */
  u8 = svtbl2 (u8x2, u8, 3); /* { dg-error {too many arguments to function 'svtbl2'} } */
  u8 = svtbl2 (u8, u8); /* { dg-error {passing single vector 'svuint8_t' to argument 1 of 'svtbl2', which expects a tuple of 2 vectors} } */
  u8 = svtbl2 (u8x3, u8); /* { dg-error {passing 'svuint8x3_t' to argument 1 of 'svtbl2', which expects a tuple of 2 vectors} } */
  u8 = svtbl2 (pg, u8); /* { dg-error {passing 'svbool_t' to argument 1 of 'svtbl2', which expects a tuple of 2 vectors} } */
  u8 = svtbl2 (u8x2, u8x2); /* { dg-error {passing 'svuint8x2_t' to argument 2 of 'svtbl2', which expects a single SVE vector rather than a tuple} } */
  u8 = svtbl2 (u8x2, f64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */
  u8 = svtbl2 (u8x2, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */
  u8 = svtbl2 (u8x2, u8);
  u8 = svtbl2 (u8x2, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */
  s8 = svtbl2 (s8x2, f64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */
  s8 = svtbl2 (s8x2, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */
  s8 = svtbl2 (s8x2, u8);
  s8 = svtbl2 (s8x2, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl2', which expects a vector of unsigned integers} } */

  return f64;
}
