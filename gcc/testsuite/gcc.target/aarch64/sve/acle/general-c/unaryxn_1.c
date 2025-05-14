#include <arm_sve.h>

#pragma GCC target "+sve2+sme2"

void
test (svfloat32_t f32, svfloat32x2_t f32x2, svfloat32x3_t f32x3,
      svfloat32x4_t f32x4) __arm_streaming
{
  svuzp (); /* { dg-error {too few arguments to function 'svuzp'} } */
  svuzp (f32x2, f32x2); /* { dg-error {too many arguments to function 'svuzp'} } */
  svuzp (f32); /* { dg-error {svuzp' has no form that takes 'svfloat32_t' arguments} } */
  svuzp (f32x2);
  svuzp (f32x3); /* { dg-error {svuzp' has no form that takes 'svfloat32x3_t' arguments} } */
  svuzp (f32x4);
}
