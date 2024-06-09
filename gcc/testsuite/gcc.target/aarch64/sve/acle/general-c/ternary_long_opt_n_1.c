/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svuint16_t u16, svuint32_t u32,
    svfloat16_t f16, svfloat32_t f32)
{
  svabalb (u16, u8); /* { dg-error {too few arguments to function 'svabalb'} } */
  svabalb (u16, u8, u8, u8); /* { dg-error {too many arguments to function 'svabalb'} } */
  svabalb (0, u8, u8); /* { dg-error {passing 'int' to argument 1 of 'svabalb', which expects an SVE type rather than a scalar} } */
  svabalb (pg, u8, u8); /* { dg-error {'svabalb' has no form that takes 'svbool_t' arguments} } */
  svabalb (u8, u8, u8); /* { dg-error {'svabalb' has no form that takes 'svuint8_t' arguments} } */
  svabalb (f16, u8, u8); /* { dg-error {'svabalb' has no form that takes 'svfloat16_t' arguments} } */
  svabalb (f32, f16, f16); /* { dg-error {'svabalb' has no form that takes 'svfloat32_t' arguments} } */
  svabalb (u16, u8, u8);
  svabalb (u16, 0, u8); /* { dg-error {passing 'int' to argument 2 of 'svabalb', which expects an SVE type rather than a scalar} } */
  svabalb (u16, s8, u8); /* { dg-error {arguments 1 and 2 of 'svabalb' must have the same signedness, but the values passed here have type 'svuint16_t' and 'svint8_t' respectively} } */
  svabalb (u16, u8, 0);
  svabalb (u16, u8, s8); /* { dg-error {arguments 1 and 3 of 'svabalb' must have the same signedness, but the values passed here have type 'svuint16_t' and 'svint8_t' respectively} } */
  svabalb (u16, u16, u16); /* { dg-error {passing 'svuint16_t' instead of the expected 'svuint8_t' to argument 2 of 'svabalb', after passing 'svuint16_t' to argument 1} } */
  svabalb (u32, u8, u16); /* { dg-error {passing 'svuint8_t' instead of the expected 'svuint16_t' to argument 2 of 'svabalb', after passing 'svuint32_t' to argument 1} } */
  svabalb (u32, u16, u8); /* { dg-error {passing 'svuint8_t' instead of the expected 'svuint16_t' to argument 3 of 'svabalb', after passing 'svuint32_t' to argument 1} } */
}
