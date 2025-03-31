/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sve2+sme2"

void
f1 (svcount_t pn, svfloat16_t f16, svint16_t s16, svfloat32_t f32,
    svfloat16x2_t f16x2, svfloat16x3_t f16x3, svfloat16x4_t f16x4)
  __arm_streaming
{
  svclamp (f16, f16); /* { dg-error {too few arguments to function 'svclamp'} } */
  svclamp (f16, f16, f16, f16); /* { dg-error {too many arguments to function 'svclamp'} } */
  svclamp (0, f16, f16); /* { dg-error {passing 'int' to argument 1 of 'svclamp', which expects an SVE type rather than a scalar type} } */
  svclamp (f16, f16, f16);
  svclamp (s16, s16, s16);
  svclamp (pn, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svclamp', but argument 1 had type 'svcount_t'} } */
  svclamp (f16, s16, f16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svclamp', but argument 1 had type 'svfloat16_t'} } */
  svclamp (f16, f32, f32); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svclamp', but argument 1 had type 'svfloat16_t'} } */
  svclamp (f16, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svclamp', but argument 1 had type 'svfloat16_t'} } */
  svclamp (f16, f16, 0); /* { dg-error {passing 'int' to argument 3 of 'svclamp', which expects an SVE type rather than a scalar} } */
  svclamp (f16, f16x2, f16); /* { dg-error {passing 'svfloat16x2_t' to argument 2 of 'svclamp', which expects a single SVE vector rather than a tuple} } */
  svclamp (f16, f16x4, f16); /* { dg-error {passing 'svfloat16x4_t' to argument 2 of 'svclamp', which expects a single SVE vector rather than a tuple} } */
  svclamp (f16, f16, f16x2); /* { dg-error {passing 'svfloat16x2_t' to argument 3 of 'svclamp', which expects a single SVE vector rather than a tuple} } */
  svclamp (f16, f16, f16x3); /* { dg-error {passing 'svfloat16x3_t' to argument 3 of 'svclamp', which expects a single SVE vector rather than a tuple} } */

  svclamp (f16x2, f16x2, f16x2); /* { dg-error {passing 'svfloat16x2_t' to argument 2 of 'svclamp', which expects a single SVE vector rather than a tuple} } */
  svclamp (f16x2, s16, f16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svclamp', but argument 1 was a tuple of 'svfloat16_t'} } */
  svclamp (f16x2, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svclamp', but argument 1 was a tuple of 'svfloat16_t'} } */
}
