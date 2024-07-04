/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svint8_t s8, svint64_t s64, svuint8_t u8, svuint16_t u16,
    svfloat32_t f32, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svwrite_ver_za8_m (0, 0, pg); /* { dg-error {too few arguments to function 'svwrite_ver_za8_m'} } */
  svwrite_ver_za8_m (0, 0, pg, s8, 0); /* { dg-error {too many arguments to function 'svwrite_ver_za8_m'} } */
  svwrite_ver_za8_m (tile, 0, pg, s8); /* { dg-error {argument 1 of 'svwrite_ver_za8_m' must be an integer constant expression} } */
  svwrite_ver_za8_m (-1, 0, pg, s8); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za8_m', which expects the value 0} } */
  svwrite_ver_za8_m (1, 0, pg, s8); /* { dg-error {passing 1 to argument 1 of 'svwrite_ver_za8_m', which expects the value 0} } */
  svwrite_ver_za8_m (0, u8, pg, s8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svwrite_ver_za8_m', which expects 'uint32_t'} } */
  svwrite_ver_za8_m (0, 0, s8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svwrite_ver_za8_m', which expects 'svbool_t'} } */
  svwrite_ver_za8_m (0, 0, pg, tile); /* { dg-error {passing 'uint32_t'.* to argument 4 of 'svwrite_ver_za8_m', which expects an SVE type} } */
  svwrite_ver_za8_m (0, 0, pg, pg); /* { dg-error {'svwrite_ver_za8_m' has no form that takes 'svbool_t' arguments} } */
  svwrite_ver_za8_m (0, 0, pg, u16); /* { dg-error {'svwrite_ver_za8_m' has no form that takes 'svuint16_t' arguments} } */

  svwrite_ver_za16_m (-1, 0, pg, u16); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za16_m', which expects a value in the range \[0, 1\]} } */
  svwrite_ver_za16_m (2, 0, pg, u16); /* { dg-error {passing 2 to argument 1 of 'svwrite_ver_za16_m', which expects a value in the range \[0, 1\]} } */

  svwrite_ver_za32_m (-1, 0, pg, f32); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za32_m', which expects a value in the range \[0, 3\]} } */
  svwrite_ver_za32_m (4, 0, pg, f32); /* { dg-error {passing 4 to argument 1 of 'svwrite_ver_za32_m', which expects a value in the range \[0, 3\]} } */

  svwrite_ver_za64_m (-1, 0, pg, s64); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za64_m', which expects a value in the range \[0, 7\]} } */
  svwrite_ver_za64_m (8, 0, pg, s64); /* { dg-error {passing 8 to argument 1 of 'svwrite_ver_za64_m', which expects a value in the range \[0, 7\]} } */

  svwrite_ver_za128_m (-1, 0, pg, s8); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za128_m', which expects a value in the range \[0, 15\]} } */
  svwrite_ver_za128_m (16, 0, pg, s8); /* { dg-error {passing 16 to argument 1 of 'svwrite_ver_za128_m', which expects a value in the range \[0, 15\]} } */
  svwrite_ver_za128_m (-1, 0, pg, f32); /* { dg-error {passing -1 to argument 1 of 'svwrite_ver_za128_m', which expects a value in the range \[0, 15\]} } */
  svwrite_ver_za128_m (16, 0, pg, f32); /* { dg-error {passing 16 to argument 1 of 'svwrite_ver_za128_m', which expects a value in the range \[0, 15\]} } */
}

void
f2 (svbool_t pg, svint8_t s8) __arm_streaming
{
  svwrite_ver_za8_m (0, 0, pg, s8); /* { dg-error {ACLE function 'svwrite_ver_za8_s8_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint8_t s8) __arm_inout("za")
{
  svwrite_ver_za8_m (0, 0, pg, s8); /* { dg-error {ACLE function 'svwrite_ver_za8_s8_m' can only be called when SME streaming mode is enabled} } */
}
