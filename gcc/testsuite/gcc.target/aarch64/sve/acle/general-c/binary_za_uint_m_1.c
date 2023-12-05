/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svsumopa_za32_m (0, pg, pg, s8); /* { dg-error {too few arguments to function 'svsumopa_za32_m'} } */
  svsumopa_za32_m (0, pg, pg, s8, u8, 0); /* { dg-error {too many arguments to function 'svsumopa_za32_m'} } */
  svsumopa_za32_m (tile, pg, pg, s8, u8); /* { dg-error {argument 1 of 'svsumopa_za32_m' must be an integer constant expression} } */
  svsumopa_za32_m (-1, pg, pg, s8, u8); /* { dg-error {passing -1 to argument 1 of 'svsumopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svsumopa_za32_m (4, pg, pg, s8, u8); /* { dg-error {passing 4 to argument 1 of 'svsumopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svsumopa_za32_m (0, u8, pg, s8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svsumopa_za32_m', which expects 'svbool_t'} } */
  svsumopa_za32_m (0, pg, u8, s8, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svsumopa_za32_m', which expects 'svbool_t'} } */
  svsumopa_za32_m (0, pg, pg, tile, s8); /* { dg-error {passing 'uint32_t'.* to argument 4 of 'svsumopa_za32_m', which expects an SVE type} } */
  svsumopa_za32_m (0, pg, pg, u8, u8); /* { dg-error {'svsumopa_za32_m' has no form that takes 'svuint8_t' arguments} } */
  svsumopa_za32_m (0, pg, pg, pg, u8); /* { dg-error {'svsumopa_za32_m' has no form that takes 'svbool_t' arguments} } */
  svsumopa_za32_m (0, pg, pg, f16, u8); /* { dg-error {'svsumopa_za32_m' has no form that takes 'svfloat16_t' arguments} } */
  svsumopa_za32_m (0, pg, pg, s8, s8); /* { dg-error {passing 'svint8_t' to argument 5 of 'svsumopa_za32_m', which expects a vector of unsigned integers} } */
  svsumopa_za32_m (0, pg, pg, s8, u16); /* { dg-error {arguments 4 and 5 of 'svsumopa_za32_m' must have the same element size, but the values passed here have type 'svint8_t' and 'svuint16_t' respectively} } */
  svsumopa_za32_m (0, pg, pg, s16, u16); /* { dg-error {'svsumopa_za32_m' has no form that takes 'svint16_t' arguments} } */

  svsumopa_za64_m (0, pg, pg, s16, u16); /* { dg-error {ACLE function 'svsumopa_za64_s16_m' requires ISA extension 'sme-i16i64'} } */
}

void
f2 (svbool_t pg, svint8_t s8, svuint8_t u8) __arm_streaming
{
  svsumopa_za32_m (0, pg, pg, s8, u8); /* { dg-error {ACLE function 'svsumopa_za32_s8_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint8_t s8, svuint8_t u8) __arm_inout("za")
{
  svsumopa_za32_m (0, pg, pg, s8, u8); /* { dg-error {ACLE function 'svsumopa_za32_s8_m' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("arch=armv9-a+sme-i16i64")

void
f4 (svbool_t pg, svint16_t s16, svuint16_t u16)
  __arm_streaming __arm_inout("za")
{
  svsumopa_za64_m (-1, pg, pg, s16, u16); /* { dg-error {passing -1 to argument 1 of 'svsumopa_za64_m', which expects a value in the range \[0, 7\]} } */
  svsumopa_za64_m (8, pg, pg, s16, u16); /* { dg-error {passing 8 to argument 1 of 'svsumopa_za64_m', which expects a value in the range \[0, 7\]} } */
}
