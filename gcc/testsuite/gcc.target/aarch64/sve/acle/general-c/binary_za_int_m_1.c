/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svusmopa_za32_m (0, pg, pg, u8); /* { dg-error {too few arguments to function 'svusmopa_za32_m'} } */
  svusmopa_za32_m (0, pg, pg, u8, s8, 0); /* { dg-error {too many arguments to function 'svusmopa_za32_m'} } */
  svusmopa_za32_m (tile, pg, pg, u8, s8); /* { dg-error {argument 1 of 'svusmopa_za32_m' must be an integer constant expression} } */
  svusmopa_za32_m (-1, pg, pg, u8, s8); /* { dg-error {passing -1 to argument 1 of 'svusmopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svusmopa_za32_m (4, pg, pg, u8, s8); /* { dg-error {passing 4 to argument 1 of 'svusmopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svusmopa_za32_m (0, u8, pg, u8, s8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svusmopa_za32_m', which expects 'svbool_t'} } */
  svusmopa_za32_m (0, pg, u8, u8, s8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svusmopa_za32_m', which expects 'svbool_t'} } */
  svusmopa_za32_m (0, pg, pg, tile, s8); /* { dg-error {passing 'uint32_t'.* to argument 4 of 'svusmopa_za32_m', which expects an SVE type} } */
  svusmopa_za32_m (0, pg, pg, s8, s8); /* { dg-error {'svusmopa_za32_m' has no form that takes 'svint8_t' arguments} } */
  svusmopa_za32_m (0, pg, pg, pg, s8); /* { dg-error {'svusmopa_za32_m' has no form that takes 'svbool_t' arguments} } */
  svusmopa_za32_m (0, pg, pg, f16, s8); /* { dg-error {'svusmopa_za32_m' has no form that takes 'svfloat16_t' arguments} } */
  svusmopa_za32_m (0, pg, pg, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 5 of 'svusmopa_za32_m', which expects a vector of signed integers} } */
  svusmopa_za32_m (0, pg, pg, u8, s16); /* { dg-error {arguments 4 and 5 of 'svusmopa_za32_m' must have the same element size, but the values passed here have type 'svuint8_t' and 'svint16_t' respectively} } */
  svusmopa_za32_m (0, pg, pg, u16, s16); /* { dg-error {'svusmopa_za32_m' has no form that takes 'svuint16_t' arguments} } */

  svusmopa_za64_m (0, pg, pg, u16, s16); /* { dg-error {ACLE function 'svusmopa_za64_u16_m' requires ISA extension 'sme-i16i64'} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8) __arm_streaming
{
  svusmopa_za32_m (0, pg, pg, u8, s8); /* { dg-error {ACLE function 'svusmopa_za32_u8_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint8_t s8, svuint8_t u8) __arm_inout("za")
{
  svusmopa_za32_m (0, pg, pg, u8, s8); /* { dg-error {ACLE function 'svusmopa_za32_u8_m' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("arch=armv9-a+sme-i16i64")

void
f4 (svbool_t pg, svint16_t s16, svuint16_t u16)
  __arm_streaming __arm_inout("za")
{
  svusmopa_za64_m (-1, pg, pg, u16, s16); /* { dg-error {passing -1 to argument 1 of 'svusmopa_za64_m', which expects a value in the range \[0, 7\]} } */
  svusmopa_za64_m (8, pg, pg, u16, s16); /* { dg-error {passing 8 to argument 1 of 'svusmopa_za64_m', which expects a value in the range \[0, 7\]} } */
}
