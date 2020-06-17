/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64,
    svfloat16_t f16)
{
  svaddlb (u16); /* { dg-error {too few arguments to function 'svaddlb'} } */
  svaddlb (u16, u16, u16); /* { dg-error {too many arguments to function 'svaddlb'} } */
  svaddlb (pg, pg); /* { dg-error {'svaddlb' has no form that takes 'svbool_t' arguments} } */
  svaddlb (u8, u8);
  svaddlb (s8, s8);
  svaddlb (u16, u16);
  svaddlb (s16, s16);
  svaddlb (u32, u32);
  svaddlb (s32, s32);
  svaddlb (u64, u64); /* { dg-error {'svaddlb' has no form that takes 'svuint64_t' arguments} } */
  svaddlb (s64, s64); /* { dg-error {'svaddlb' has no form that takes 'svint64_t' arguments} } */
  svaddlb (f16, f16); /* { dg-error {'svaddlb' has no form that takes 'svfloat16_t' arguments} } */
  svaddlb (1, u8); /* { dg-error {passing 'int' to argument 1 of 'svaddlb', which expects an SVE vector type} } */
  svaddlb (u8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svaddlb', but previous arguments had type 'svuint8_t'} } */
  svaddlb (u8, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svaddlb', but previous arguments had type 'svuint8_t'} } */
  svaddlb (u8, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svaddlb', but previous arguments had type 'svuint8_t'} } */
  svaddlb (u16, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svaddlb', but previous arguments had type 'svuint16_t'} } */
  svaddlb (u8, 0);
  svaddlb (u16, 0);
  svaddlb (u32, 0);
  svaddlb (u64, 0); /* { dg-error {'svaddlb' has no form that takes 'svuint64_t' arguments} } */
  svaddlb (pg, 0); /* { dg-error {'svaddlb' has no form that takes 'svbool_t' arguments} } */
}
