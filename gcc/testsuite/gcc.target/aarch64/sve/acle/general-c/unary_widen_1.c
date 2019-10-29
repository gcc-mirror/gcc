#include <arm_sve.h>

void
test (svbool_t pg, svint8_t s8, svuint8_t u8,
      svint16_t s16, svuint16_t u16, svfloat16_t f16,
      svint32_t s32, svuint32_t u32, svfloat32_t f32,
      svint64_t s64, svuint64_t u64, svfloat64_t f64, float f, int i)
{
  svunpklo (); /* { dg-error {too few arguments to function 'svunpklo'} } */
  svunpklo (pg, s8); /* { dg-error {too many arguments to function 'svunpklo'} } */
  svunpklo (i); /* { dg-error {passing 'int' to argument 1 of 'svunpklo', which expects an SVE vector type} } */
  svunpklo (f); /* { dg-error {passing 'float' to argument 1 of 'svunpklo', which expects an SVE vector type} } */
  svunpklo (pg);
  svunpklo (s8);
  svunpklo (s16);
  svunpklo (s32);
  svunpklo (s64); /* { dg-error {'svunpklo' has no form that takes 'svint64_t' arguments} } */
  svunpklo (u8);
  svunpklo (u16);
  svunpklo (u32);
  svunpklo (u64); /* { dg-error {'svunpklo' has no form that takes 'svuint64_t' arguments} } */
  svunpklo (f16); /* { dg-error {'svunpklo' has no form that takes 'svfloat16_t' arguments} } */
  svunpklo (f32); /* { dg-error {'svunpklo' has no form that takes 'svfloat32_t' arguments} } */
  svunpklo (f64); /* { dg-error {'svunpklo' has no form that takes 'svfloat64_t' arguments} } */
}
