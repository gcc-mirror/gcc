/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve -std=c99 -Wall -Wextra" } */

#include <arm_neon_sve_bridge.h>

float64x2_t
f1 (int8x16_t s8, svint8_t sveS8, svint8x2_t sveS8x2, svint64_t sveS64,
    int64x2_t s64, svbfloat16_t sveBF16, bfloat16x8_t bf16, int8x8_t s8_64bit,
    svbool_t svbool)
{
  float64x2_t f64;

  sveS8 = svset_neonq (sveS8, s8);
  sveS64 = svset_neonq (sveS64, s64);
  sveBF16 = svset_neonq (sveBF16, bf16);
  sveS8 = svset_neonq (); /* { dg-error {too few arguments to function 'svset_neonq'} } */
  sveS8 = svset_neonq (sveS8, s8, 1); /* { dg-error {too many arguments to function 'svset_neonq'} } */
  sveS8 = svset_neonq (s8, s8); /* { dg-error {incompatible type for argument 1 of 'svset_neonq_s8'} } */
  f64 = svset_neonq (sveS8, s8); /* { dg-error {incompatible types when assigning to type 'float64x2_t' from type 'svint8_t'} } */
  sveS8 = svset_neonq (sveS8x2, s8); /* { dg-error {incompatible type for argument 1 of 'svset_neonq_s8'} } */
  sveS8 = svset_neonq (sveS8, sveS8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svset_neonq', which expects a 128 bit NEON vector type} } */
  sveS8 = svset_neonq (sveS8, s8_64bit); /* { dg-error {passing 'int8x8_t' to argument 2 of 'svset_neonq', which expects a 128 bit NEON vector type} } */
  sveS8 = svset_neonq (sveS64, s64); /* { dg-error {incompatible types when assigning to type 'svint8_t' from type 'svint64_t} } */
  sveS8 = svset_neonq (svbool, svbool); /* { dg-error {passing 'svbool_t' to argument 2 of 'svset_neonq', which expects a 128 bit NEON vector type} } */

  return f64;
}