/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8, svint8_t s8, svuint16_t u16, svint16_t s16,
    svfloat16_t f16)
{
  svtbl (u8); /* { dg-error {too few arguments to function 'svtbl'} } */
  svtbl (u8, u8, u8); /* { dg-error {too many arguments to function 'svtbl'} } */
  svtbl (pg, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (pg, u8); /* { dg-error {'svtbl' has no form that takes 'svbool_t' arguments} } */

  svtbl (u8, 0); /* { dg-error {passing 'int' to argument 2 of 'svtbl', which expects an SVE type rather than a scalar} } */
  svtbl (u8, u8);
  svtbl (u8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (u8, u16); /* { dg-error {arguments 1 and 2 of 'svtbl' must have the same element size, but the values passed here have type 'svuint8_t' and 'svuint16_t' respectively} } */
  svtbl (u8, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (u8, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */

  svtbl (s8, u8);
  svtbl (s8, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (s8, u16); /* { dg-error {arguments 1 and 2 of 'svtbl' must have the same element size, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svtbl (s8, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (s8, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */

  svtbl (u16, u8); /* { dg-error {arguments 1 and 2 of 'svtbl' must have the same element size, but the values passed here have type 'svuint16_t' and 'svuint8_t' respectively} } */
  svtbl (u16, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (u16, u16);
  svtbl (u16, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (u16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */

  svtbl (s16, u8); /* { dg-error {arguments 1 and 2 of 'svtbl' must have the same element size, but the values passed here have type 'svint16_t' and 'svuint8_t' respectively} } */
  svtbl (s16, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (s16, u16);
  svtbl (s16, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (s16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */

  svtbl (f16, u8); /* { dg-error {arguments 1 and 2 of 'svtbl' must have the same element size, but the values passed here have type 'svfloat16_t' and 'svuint8_t' respectively} } */
  svtbl (f16, s8); /* { dg-error {passing 'svint8_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (f16, u16);
  svtbl (f16, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
  svtbl (f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svtbl', which expects a vector of unsigned integers} } */
}
