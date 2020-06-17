/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svuint16_t u16, svint16_t s16,
    svfloat16_t f16)
{
  svtbx (u8, u8); /* { dg-error {too few arguments to function 'svtbx'} } */
  svtbx (u8, u8, u8, u8); /* { dg-error {too many arguments to function 'svtbx'} } */
  svtbx (pg, pg, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (pg, pg, u8); /* { dg-error {'svtbx' has no form that takes 'svbool_t' arguments} } */

  svtbx (u8, 0, u8); /* { dg-error {passing 'int' to argument 2 of 'svtbx', which expects an SVE vector type} } */
  svtbx (u8, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svtbx', which expects an SVE vector type} } */
  svtbx (u8, s8, u8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbx', but previous arguments had type 'svuint8_t'} } */
  svtbx (u8, u8, u8);
  svtbx (u8, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (u8, u8, u16); /* { dg-error {arguments 1 and 3 of 'svtbx' must have the same element size, but the values passed here have type 'svuint8_t' and 'svuint16_t' respectively} } */
  svtbx (u8, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (u8, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */

  svtbx (s8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svtbx', but previous arguments had type 'svint8_t'} } */
  svtbx (s8, s8, u8);
  svtbx (s8, s8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (s8, s8, u16); /* { dg-error {arguments 1 and 3 of 'svtbx' must have the same element size, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svtbx (s8, s8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (s8, s8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */

  svtbx (u16, 0, u16); /* { dg-error {passing 'int' to argument 2 of 'svtbx', which expects an SVE vector type} } */
  svtbx (u16, u16, u8); /* { dg-error {arguments 1 and 3 of 'svtbx' must have the same element size, but the values passed here have type 'svuint16_t' and 'svuint8_t' respectively} } */
  svtbx (u16, u16, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (u16, u16, u16);
  svtbx (u16, u16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (u16, u16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */

  svtbx (s16, u16, u16); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svtbx', but previous arguments had type 'svint16_t'} } */
  svtbx (s16, s16, u8); /* { dg-error {arguments 1 and 3 of 'svtbx' must have the same element size, but the values passed here have type 'svint16_t' and 'svuint8_t' respectively} } */
  svtbx (s16, s16, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (s16, s16, u16);
  svtbx (s16, s16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (s16, s16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */

  svtbx (f16, f16, u8); /* { dg-error {arguments 1 and 3 of 'svtbx' must have the same element size, but the values passed here have type 'svfloat16_t' and 'svuint8_t' respectively} } */
  svtbx (f16, f16, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (f16, f16, u16);
  svtbx (f16, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
  svtbx (f16, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svtbx', which expects a vector of unsigned integers} } */
}
