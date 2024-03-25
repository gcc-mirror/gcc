/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64,
    svfloat32_t f32)
{
  svqxtnb (); /* { dg-error {too few arguments to function 'svqxtnb'} } */
  svqxtnb (u16, u16); /* { dg-error {too many arguments to function 'svqxtnb'} } */
  svqxtnb (pg); /* { dg-error {'svqxtnb' has no form that takes 'svbool_t' arguments} } */
  svqxtnb (u8); /* { dg-error {'svqxtnb' has no form that takes 'svuint8_t' arguments} } */
  svqxtnb (s8); /* { dg-error {'svqxtnb' has no form that takes 'svint8_t' arguments} } */
  svqxtnb (u16);
  svqxtnb (s16);
  svqxtnb (u32);
  svqxtnb (s32);
  svqxtnb (u64);
  svqxtnb (s64);
  svqxtnb (f32); /* { dg-error {'svqxtnb' has no form that takes 'svfloat32_t' arguments} } */
  svqxtnb (1); /* { dg-error {passing 'int' to argument 1 of 'svqxtnb', which expects an SVE type rather than a scalar} } */
}
