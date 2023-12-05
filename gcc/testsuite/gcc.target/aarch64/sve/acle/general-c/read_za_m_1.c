/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svint8_t s8, svint64_t s64, svuint8_t u8, svuint16_t u16,
    svfloat32_t f32, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svread_hor_za8_m (s8, pg, 0); /* { dg-error {too few arguments to function 'svread_hor_za8_m'} } */
  svread_hor_za8_m (s8, pg, 0, 0, 0); /* { dg-error {too many arguments to function 'svread_hor_za8_m'} } */
  svread_hor_za8_m (tile, pg, 0, 0); /* { dg-error {passing 'uint32_t'.* to argument 1 of 'svread_hor_za8_m', which expects an SVE type} } */
  svread_hor_za8_m (pg, pg, 0, 0); /* { dg-error {'svread_hor_za8_m' has no form that takes 'svbool_t' arguments} } */
  svread_hor_za8_m (u16, pg, 0, 0); /* { dg-error {'svread_hor_za8_m' has no form that takes 'svuint16_t' arguments} } */
  svread_hor_za8_m (s8, s8, 0, 0); /* { dg-error {passing 'svint8_t' to argument 2 of 'svread_hor_za8_m', which expects 'svbool_t'} } */
  svread_hor_za8_m (s8, pg, tile, 0); /* { dg-error {argument 3 of 'svread_hor_za8_m' must be an integer constant expression} } */
  svread_hor_za8_m (s8, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za8_m', which expects the value 0} } */
  svread_hor_za8_m (s8, pg, 1, 0); /* { dg-error {passing 1 to argument 3 of 'svread_hor_za8_m', which expects the value 0} } */
  svread_hor_za8_m (s8, pg, 0, u8); /* { dg-error {passing 'svuint8_t' to argument 4 of 'svread_hor_za8_m', which expects 'uint32_t'} } */

  svread_hor_za16_m (u16, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za16_m', which expects a value in the range \[0, 1\]} } */
  svread_hor_za16_m (u16, pg, 2, 0); /* { dg-error {passing 2 to argument 3 of 'svread_hor_za16_m', which expects a value in the range \[0, 1\]} } */

  svread_hor_za32_m (f32, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za32_m', which expects a value in the range \[0, 3\]} } */
  svread_hor_za32_m (f32, pg, 4, 0); /* { dg-error {passing 4 to argument 3 of 'svread_hor_za32_m', which expects a value in the range \[0, 3\]} } */

  svread_hor_za64_m (s64, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za64_m', which expects a value in the range \[0, 7\]} } */
  svread_hor_za64_m (s64, pg, 8, 0); /* { dg-error {passing 8 to argument 3 of 'svread_hor_za64_m', which expects a value in the range \[0, 7\]} } */

  svread_hor_za128_m (s8, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za128_m', which expects a value in the range \[0, 15\]} } */
  svread_hor_za128_m (s8, pg, 16, 0); /* { dg-error {passing 16 to argument 3 of 'svread_hor_za128_m', which expects a value in the range \[0, 15\]} } */
  svread_hor_za128_m (f32, pg, -1, 0); /* { dg-error {passing -1 to argument 3 of 'svread_hor_za128_m', which expects a value in the range \[0, 15\]} } */
  svread_hor_za128_m (f32, pg, 16, 0); /* { dg-error {passing 16 to argument 3 of 'svread_hor_za128_m', which expects a value in the range \[0, 15\]} } */
}

void
f2 (svbool_t pg, svint8_t s8) __arm_streaming
{
  svread_hor_za8_m (s8, pg, 0, 0); /* { dg-error {ACLE function 'svread_hor_za8_s8_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint8_t s8) __arm_inout("za")
{
  svread_hor_za8_m (s8, pg, 0, 0); /* { dg-error {ACLE function 'svread_hor_za8_s8_m' can only be called when SME streaming mode is enabled} } */
}
