/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svuint8_t u8, svint16_t s16, svint32_t s32, svint64_t s64,
    svfloat32_t f32, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svaddha_za32_m (0, pg, pg); /* { dg-error {too few arguments to function 'svaddha_za32_m'} } */
  svaddha_za32_m (0, pg, pg, s32, s32); /* { dg-error {too many arguments to function 'svaddha_za32_m'} } */
  svaddha_za32_m (tile, pg, pg, s32); /* { dg-error {argument 1 of 'svaddha_za32_m' must be an integer constant expression} } */
  svaddha_za32_m (-1, pg, pg, s32); /* { dg-error {passing -1 to argument 1 of 'svaddha_za32_m', which expects a value in the range \[0, 3\]} } */
  svaddha_za32_m (4, pg, pg, s32); /* { dg-error {passing 4 to argument 1 of 'svaddha_za32_m', which expects a value in the range \[0, 3\]} } */
  svaddha_za32_m (0, u8, pg, s32); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svaddha_za32_m', which expects 'svbool_t'} } */
  svaddha_za32_m (0, pg, u8, s32); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svaddha_za32_m', which expects 'svbool_t'} } */
  svaddha_za32_m (0, pg, pg, tile); /* { dg-error {passing 'uint32_t'.* to argument 4 of 'svaddha_za32_m', which expects an SVE type} } */
  svaddha_za32_m (0, pg, pg, pg); /* { dg-error {'svaddha_za32_m' has no form that takes 'svbool_t' arguments} } */
  svaddha_za32_m (0, pg, pg, u8); /* { dg-error {'svaddha_za32_m' has no form that takes 'svuint8_t' arguments} } */
  svaddha_za32_m (0, pg, pg, s16); /* { dg-error {'svaddha_za32_m' has no form that takes 'svint16_t' arguments} } */
  svaddha_za32_m (0, pg, pg, f32); /* { dg-error {'svaddha_za32_m' has no form that takes 'svfloat32_t' arguments} } */
  svaddha_za32_m (0, pg, pg, s64); /* { dg-error {'svaddha_za32_m' has no form that takes 'svint64_t' arguments} } */

  svaddha_za64_m (0, pg, pg, s64); /* { dg-error {ACLE function 'svaddha_za64_s64_m' requires ISA extension 'sme-i16i64'} } */
}

void
f2 (svbool_t pg, svint32_t s32) __arm_streaming
{
  svaddha_za32_m (0, pg, pg, s32); /* { dg-error {ACLE function 'svaddha_za32_s32_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint32_t s32) __arm_inout("za")
{
  svaddha_za32_m (0, pg, pg, s32); /* { dg-error {ACLE function 'svaddha_za32_s32_m' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("arch=armv9-a+sme-i16i64")

void
f4 (svbool_t pg, svint64_t s64)
  __arm_streaming __arm_inout("za")
{
  svaddha_za64_m (-1, pg, pg, s64); /* { dg-error {passing -1 to argument 1 of 'svaddha_za64_m', which expects a value in the range \[0, 7\]} } */
  svaddha_za64_m (8, pg, pg, s64); /* { dg-error {passing 8 to argument 1 of 'svaddha_za64_m', which expects a value in the range \[0, 7\]} } */
}
