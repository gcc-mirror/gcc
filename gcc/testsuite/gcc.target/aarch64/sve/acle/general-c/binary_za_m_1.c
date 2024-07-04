/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8, svint16_t s16, svint32_t s32,
    svfloat16_t f16, svfloat32_t f32, svfloat64_t f64, uint32_t tile)
  __arm_streaming __arm_inout("za")
{
  svmopa_za32_m (0, pg, pg, s8); /* { dg-error {too few arguments to function 'svmopa_za32_m'} } */
  svmopa_za32_m (0, pg, pg, s8, s8, 0); /* { dg-error {too many arguments to function 'svmopa_za32_m'} } */
  svmopa_za32_m (tile, pg, pg, s8, s8); /* { dg-error {argument 1 of 'svmopa_za32_m' must be an integer constant expression} } */
  svmopa_za32_m (-1, pg, pg, s8, s8); /* { dg-error {passing -1 to argument 1 of 'svmopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svmopa_za32_m (4, pg, pg, s8, s8); /* { dg-error {passing 4 to argument 1 of 'svmopa_za32_m', which expects a value in the range \[0, 3\]} } */
  svmopa_za32_m (0, u8, pg, s8, s8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svmopa_za32_m', which expects 'svbool_t'} } */
  svmopa_za32_m (0, pg, u8, s8, s8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svmopa_za32_m', which expects 'svbool_t'} } */
  svmopa_za32_m (0, pg, pg, tile, s8); /* { dg-error {passing 'uint32_t'.* to argument 4 of 'svmopa_za32_m', which expects an SVE type} } */
  svmopa_za32_m (0, pg, pg, u8, s8); /* { dg-error {passing 'svint8_t'.* to argument 5 of 'svmopa_za32_m', but argument 4 had type 'svuint8_t'} } */
  svmopa_za32_m (0, pg, pg, s8, f16); /* { dg-error {passing 'svfloat16_t'.* to argument 5 of 'svmopa_za32_m', but argument 4 had type 'svint8_t'} } */
  svmopa_za32_m (0, pg, pg, pg, pg); /* { dg-error {'svmopa_za32_m' has no form that takes 'svbool_t' arguments} } */
  svmopa_za32_m (0, pg, pg, s32, s32); /* { dg-error {'svmopa_za32_m' has no form that takes 'svint32_t' arguments} } */
  svmopa_za32_m (0, pg, pg, f64, f64); /* { dg-error {'svmopa_za32_m' has no form that takes 'svfloat64_t' arguments} } */

  svmopa_za64_m (0, pg, pg, s16, s16); /* { dg-error {ACLE function 'svmopa_za64_s16_m' requires ISA extension 'sme-i16i64'} } */
}

void
f2 (svbool_t pg, svint8_t s8) __arm_streaming
{
  svmopa_za32_m (0, pg, pg, s8, s8); /* { dg-error {ACLE function 'svmopa_za32_s8_m' can only be called from a function that has 'za' state} } */
}

void
f3 (svbool_t pg, svint8_t s8)  __arm_inout("za")
{
  svmopa_za32_m (0, pg, pg, s8, s8); /* { dg-error {ACLE function 'svmopa_za32_s8_m' can only be called when SME streaming mode is enabled} } */
}

#pragma GCC target ("arch=armv9-a+sme-i16i64")

void
f4 (svbool_t pg, svint16_t s16) __arm_streaming __arm_inout("za")
{
  svmopa_za64_m (-1, pg, pg, s16, s16); /* { dg-error {passing -1 to argument 1 of 'svmopa_za64_m', which expects a value in the range \[0, 7\]} } */
  svmopa_za64_m (8, pg, pg, s16, s16); /* { dg-error {passing 8 to argument 1 of 'svmopa_za64_m', which expects a value in the range \[0, 7\]} } */
}
