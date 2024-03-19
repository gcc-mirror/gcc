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
  svqxtunb (); /* { dg-error {too few arguments to function 'svqxtunb'} } */
  svqxtunb (u16, u16); /* { dg-error {too many arguments to function 'svqxtunb'} } */
  svqxtunb (pg); /* { dg-error {'svqxtunb' has no form that takes 'svbool_t' arguments} } */
  svqxtunb (u8); /* { dg-error {'svqxtunb' has no form that takes 'svuint8_t' arguments} } */
  svqxtunb (s8); /* { dg-error {'svqxtunb' has no form that takes 'svint8_t' arguments} } */
  svqxtunb (u16); /* { dg-error {'svqxtunb' has no form that takes 'svuint16_t' arguments} } */
  svqxtunb (s16);
  svqxtunb (u32); /* { dg-error {'svqxtunb' has no form that takes 'svuint32_t' arguments} } */
  svqxtunb (s32);
  svqxtunb (u64); /* { dg-error {'svqxtunb' has no form that takes 'svuint64_t' arguments} } */
  svqxtunb (s64);
  svqxtunb (f32); /* { dg-error {'svqxtunb' has no form that takes 'svfloat32_t' arguments} } */
  svqxtunb (1); /* { dg-error {passing 'int' to argument 1 of 'svqxtunb', which expects an SVE type rather than a scalar} } */
}
