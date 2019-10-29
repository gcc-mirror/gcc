/* { dg-do compile } */
/* { dg-additional-options "-std=c++11 -Wall -Wextra" } */

#include <arm_sve.h>

constexpr uint64_t const_sub (uint64_t a, uint64_t b) { return a - b; }
uint64_t add (uint64_t a, uint64_t b) { return a + b; }

svfloat64_t
f1 (svbool_t pg, svfloat16_t f16, svfloat16x3_t f16x3, svfloat16x4_t f16x4,
    int x)
{
  const int one = 1;
  svfloat64_t f64;

  f16 = svget3_f16 (f16x3); /* { dg-error {too few arguments to function '[^']*'} } */
  f16 = svget3_f16 (f16x3, 1, 2); /* { dg-error {too many arguments to function '[^']*'} } */
  f16 = svget3_f16 (f16, 0); /* { dg-error {cannot convert 'svfloat16_t' to 'svfloat16x3_t'} } */
  f16 = svget3_f16 (f16x4, 0); /* { dg-error {cannot convert 'svfloat16x4_t' to 'svfloat16x3_t'} } */
  f16 = svget3_f16 (pg, 0); /* { dg-error {cannot convert 'svbool_t' to 'svfloat16x3_t'} } */
  f16 = svget3_f16 (f16x3, x); /* { dg-error "argument 2 of 'svget3_f16' must be an integer constant expression" } */
  f16 = svget3_f16 (f16x3, 0);
  f64 = svget3_f16 (f16x3, 0); /* { dg-error "cannot convert 'svfloat16_t' to 'svfloat64_t' in assignment" } */
  f16 = svget3_f16 (f16x3, 1);
  f16 = svget3_f16 (f16x3, 2);
  f16 = svget3_f16 (f16x3, 3); /* { dg-error {passing 3 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, 4); /* { dg-error {passing 4 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, 5); /* { dg-error {passing 5 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, ~0U); /* { dg-error {passing [^ ]* to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, one);
  f16 = svget3_f16 (f16x3, 3 - 2);
  f16 = svget3_f16 (f16x3, 1.0);
  f16 = svget3_f16 (f16x3, const_sub (5, 4));
  f16 = svget3_f16 (f16x3, const_sub (6, 4));
  f16 = svget3_f16 (f16x3, const_sub (7, 4)); /* { dg-error {passing 3 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, const_sub (8, 4)); /* { dg-error {passing 4 to argument 2 of 'svget3_f16', which expects a value in the range \[0, 2\]} } */
  f16 = svget3_f16 (f16x3, add (0, 0)); /* { dg-error "argument 2 of 'svget3_f16' must be an integer constant expression" } */

  return f64;
}
