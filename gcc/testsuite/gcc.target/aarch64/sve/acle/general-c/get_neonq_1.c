/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve -std=c99 -Wall -Wextra" } */

#include <arm_neon_sve_bridge.h>

float64x2_t
f1 (int8x16_t s8, svint8_t sveS8, svint8x2_t sveS8x2, svint64_t sveS64)
{
  float64x2_t f64;

  s8 = svget_neonq (sveS8);
  s8 = svget_neonq (); /* { dg-error {too few arguments to function 'svget_neonq'} } */
  s8 = svget_neonq (sveS8, 1); /* { dg-error {too many arguments to function 'svget_neonq'} } */
  s8 = svget_neonq (s8); /* { dg-error {passing 'int8x16_t' to argument 1 of 'svget_neonq', which expects an SVE type} } */
  f64 = svget_neonq (sveS8); /* { dg-error {incompatible types when assigning to type 'float64x2_t' from type '__Int8x16_t'} } */
  s8 = svget_neonq (sveS8x2); /* { dg-error {passing 'svint8x2_t' to argument 1 of 'svget_neonq', which expects a single SVE vector rather than a tuple} } */
  s8 = svget_neonq (sveS64); /* { dg-error {incompatible types when assigning to type 'int8x16_t' from type '__Int64x2_t} } */

  return f64;
}