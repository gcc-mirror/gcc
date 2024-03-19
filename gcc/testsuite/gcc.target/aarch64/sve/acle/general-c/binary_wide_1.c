/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32,
    svint64_t s64, svuint64_t u64,
    svfloat16_t f16, svfloat32_t f32)
{
  svadalp_m (pg, u16); /* { dg-error {too few arguments to function 'svadalp_m'} } */
  svadalp_m (pg, u16, u8, u8); /* { dg-error {too many arguments to function 'svadalp_m'} } */
  svadalp_m (0, pg, pg); /* { dg-error {passing 'int' to argument 1 of 'svadalp_m', which expects 'svbool_t'} } */
  svadalp_m (u16, u8, u8); /* { dg-error {passing 'svuint16_t' to argument 1 of 'svadalp_m', which expects 'svbool_t'} } */
  svadalp_m (u32, u32, u16); /* { dg-error {passing 'svuint32_t' to argument 1 of 'svadalp_m', which expects 'svbool_t'} } */
  svadalp_m (pg, pg, pg); /* { dg-error {'svadalp_m' has no form that takes 'svbool_t' arguments} } */
  svadalp_m (pg, u8, u8); /* { dg-error {'svadalp_m' has no form that takes 'svuint8_t' arguments} } */
  svadalp_m (pg, s8, s8); /* { dg-error {'svadalp_m' has no form that takes 'svint8_t' arguments} } */
  svadalp_m (pg, u16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 3 of 'svadalp_m', after passing 'svuint16_t' to argument 2} } */
  svadalp_m (pg, u16, s16); /* { dg-error {passing 'svint16_t' instead of the expected 'svuint8_t' to argument 3 of 'svadalp_m', after passing 'svuint16_t' to argument 2} } */
  svadalp_m (pg, u16, u8);
  svadalp_m (pg, u16, s8); /* { dg-error {arguments 2 and 3 of 'svadalp_m' must have the same signedness, but the values passed here have type 'svuint16_t' and 'svint8_t' respectively} } */
  svadalp_m (pg, u16, pg); /* { dg-error {passing 'svbool_t' instead of the expected 'svuint8_t' to argument 3 of 'svadalp_m', after passing 'svuint16_t' to argument 2} } */
  svadalp_m (pg, s16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svint8_t' to argument 3 of 'svadalp_m', after passing 'svint16_t' to argument 2} } */
  svadalp_m (pg, s16, s16); /* { dg-error {passing 'svint16_t' instead of the expected 'svint8_t' to argument 3 of 'svadalp_m', after passing 'svint16_t' to argument 2} } */
  svadalp_m (pg, s16, u8); /* { dg-error {arguments 2 and 3 of 'svadalp_m' must have the same signedness, but the values passed here have type 'svint16_t' and 'svuint8_t' respectively} } */
  svadalp_m (pg, s16, s8);
  svadalp_m (pg, f32, f16); /* { dg-error {'svadalp_m' has no form that takes 'svfloat32_t' arguments} } */
  svadalp_m (pg, f16, f32); /* { dg-error {'svadalp_m' has no form that takes 'svfloat16_t' arguments} } */
  svadalp_m (pg, 0, u32); /* { dg-error {passing 'int' to argument 2 of 'svadalp_m', which expects an SVE type rather than a scalar} } */
  svadalp_m (pg, 0, u64); /* { dg-error {passing 'int' to argument 2 of 'svadalp_m', which expects an SVE type rather than a scalar} } */
  svadalp_m (pg, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svadalp_m', which expects an SVE type rather than a scalar} } */
  svadalp_m (pg, u16, 0); /* { dg-error {passing 'int' to argument 3 of 'svadalp_m', which expects an SVE type rather than a scalar} } */
}
