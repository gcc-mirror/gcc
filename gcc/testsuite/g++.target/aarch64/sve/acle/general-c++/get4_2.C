/* { dg-do compile } */
/* { dg-additional-options "-std=c++11 -Wall -Wextra" } */

#include <arm_sve.h>

constexpr uint64_t const_sub (uint64_t a, uint64_t b) { return a - b; }
uint64_t add (uint64_t a, uint64_t b) { return a + b; }

svfloat64_t
f1 (svbool_t pg, svint32_t s32, svint32x4_t s32x4, svint32x2_t s32x2, int x)
{
  const int one = 1;
  svfloat64_t f64;

  s32 = svget4_s32 (s32x4); /* { dg-error {too few arguments to function '[^']*'} } */
  s32 = svget4_s32 (s32x4, 1, 2); /* { dg-error {too many arguments to function '[^']*'} } */
  s32 = svget4_s32 (s32, 0); /* { dg-error {cannot convert 'svint32_t' to 'svint32x4_t'} } */
  s32 = svget4_s32 (s32x2, 0); /* { dg-error {cannot convert 'svint32x2_t' to 'svint32x4_t'} } */
  s32 = svget4_s32 (pg, 0); /* { dg-error {cannot convert 'svbool_t' to 'svint32x4_t'} } */
  s32 = svget4_s32 (s32x4, x); /* { dg-error "argument 2 of 'svget4_s32' must be an integer constant expression" } */
  s32 = svget4_s32 (s32x4, 0);
  f64 = svget4_s32 (s32x4, 0); /* { dg-error "cannot convert 'svint32_t' to 'svfloat64_t' in assignment" } */
  s32 = svget4_s32 (s32x4, 1);
  s32 = svget4_s32 (s32x4, 2);
  s32 = svget4_s32 (s32x4, 3);
  s32 = svget4_s32 (s32x4, 4); /* { dg-error {passing 4 to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, 5); /* { dg-error {passing 5 to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, ~0U); /* { dg-error {passing [^ ]* to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, one);
  s32 = svget4_s32 (s32x4, 3 - 2);
  s32 = svget4_s32 (s32x4, 1.0);
  s32 = svget4_s32 (s32x4, const_sub (5, 4));
  s32 = svget4_s32 (s32x4, const_sub (6, 4));
  s32 = svget4_s32 (s32x4, const_sub (7, 4));
  s32 = svget4_s32 (s32x4, const_sub (8, 4)); /* { dg-error {passing 4 to argument 2 of 'svget4_s32', which expects a value in the range \[0, 3\]} } */
  s32 = svget4_s32 (s32x4, add (0, 0)); /* { dg-error "argument 2 of 'svget4_s32' must be an integer constant expression" } */

  return f64;
}
