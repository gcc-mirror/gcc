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
  svaddhnb (u16); /* { dg-error {too few arguments to function 'svaddhnb'} } */
  svaddhnb (u16, u16, u16); /* { dg-error {too many arguments to function 'svaddhnb'} } */
  svaddhnb (pg, pg); /* { dg-error {'svaddhnb' has no form that takes 'svbool_t' arguments} } */
  svaddhnb (u8, u8); /* { dg-error {'svaddhnb' has no form that takes 'svuint8_t' arguments} } */
  svaddhnb (s8, s8); /* { dg-error {'svaddhnb' has no form that takes 'svint8_t' arguments} } */
  svaddhnb (u16, u16);
  svaddhnb (s16, s16);
  svaddhnb (u32, u32);
  svaddhnb (s32, s32);
  svaddhnb (u64, u64);
  svaddhnb (s64, s64);
  svaddhnb (f32, f32); /* { dg-error {'svaddhnb' has no form that takes 'svfloat32_t' arguments} } */
  svaddhnb (1, u16); /* { dg-error {passing 'int' to argument 1 of 'svaddhnb', which expects an SVE type rather than a scalar} } */
  svaddhnb (u16, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svaddhnb', but argument 1 had type 'svuint16_t'} } */
  svaddhnb (u16, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svaddhnb', but argument 1 had type 'svuint16_t'} } */
  svaddhnb (u16, u32); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svaddhnb', but argument 1 had type 'svuint16_t'} } */
  svaddhnb (u16, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svaddhnb', but argument 1 had type 'svuint16_t'} } */
  svaddhnb (u8, 0); /* { dg-error {'svaddhnb' has no form that takes 'svuint8_t' arguments} } */
  svaddhnb (u16, 0);
  svaddhnb (u32, 0);
  svaddhnb (u64, 0);
  svaddhnb (pg, 0); /* { dg-error {'svaddhnb' has no form that takes 'svbool_t' arguments} } */
}
