/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve -std=c99 -Wall -Wextra" } */

#include <arm_neon_sve_bridge.h>

float64x2_t
f1 (int8x16_t s8, svint8_t sveS8, int64x2_t s64, int8x8x2_t s8x2)
{
  float64x2_t f64;

  sveS8 = svdup_neonq (s8);
  sveS8 = svdup_neonq (); /* { dg-error {too few arguments to function 'svdup_neonq'} } */
  sveS8 = svdup_neonq (s8, 1); /* { dg-error {too many arguments to function 'svdup_neonq'} } */
  sveS8 = svdup_neonq (sveS8); /* { dg-error {passing 'svint8_t' to argument 1 of 'svdup_neonq', which expects a 128 bit NEON vector type} } */
  f64 = svdup_neonq (s8); /* { dg-error {incompatible types when assigning to type 'float64x2_t' from type 'svint8_t'} } */
  sveS8 = svdup_neonq (s8x2); /* { dg-error {passing 'int8x8x2_t' to argument 1 of 'svdup_neonq', which expects a 128 bit NEON vector type} } */
  sveS8 = svdup_neonq (s64); /* { dg-error {incompatible types when assigning to type 'svint8_t' from type 'svint64_t'} } */

  return f64;
}