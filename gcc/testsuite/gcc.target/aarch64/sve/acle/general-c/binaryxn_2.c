/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target "+sme2"

void
f1 (svbool_t pg, svcount_t pn, svuint8_t u8, svint16_t s16,
    svint8x2_t s8x2, svint8x3_t s8x3, svint8x4_t s8x4,
    svuint8x2_t u8x2, svuint8x3_t u8x3, svuint8x4_t u8x4,
    svuint16x2_t u16x2) __arm_streaming
{
  svsel (pn, u8x2); /* { dg-error {too few arguments to function 'svsel'} } */
  svsel (pn, u8x2, u8x2, u8x2); /* { dg-error {too many arguments to function 'svsel'} } */
  svsel (0, u8x2, u8x2); /* { dg-error {passing 'int' to argument 1 of 'svsel', which expects an 'svbool_t' or 'svcount_t'} } */
  svsel (u8x2, u8x2, u8x2); /* { dg-error {passing 'svuint8x2_t' to argument 1 of 'svsel', which expects an 'svbool_t' or 'svcount_t'} } */
  svsel (pg, u8x2, u8x2); /* { dg-error {operations on multiple vectors must be predicated by 'svcount_t' rather than 'svbool_t'} } */
  svsel (pn, u8x2, s8x2); /* { dg-error {passing 'svint8x2_t' to argument 3 of 'svsel', but argument 2 had type 'svuint8x2_t'} } */
  svsel (pn, u8x2, u16x2); /* { dg-error {passing 'svuint16x2_t' to argument 3 of 'svsel', but argument 2 had type 'svuint8x2_t'} } */
  svsel (pn, u8x2, 0); /* { dg-error {passing 'int' to argument 3 of 'svsel', which expects an SVE type rather than a scalar} } */
  svsel (pn, u8x2, u8); /* { dg-error {passing single vector 'svuint8_t' to argument 3 of 'svsel' after passing tuple 'svuint8x2_t' to argument 2} } */
  svsel (pn, u8x2, u8x2);
  svsel (pn, u8x2, u8x3); /* { dg-error {passing mismatched tuple types 'svuint8x2_t' and 'svuint8x3_t' to arguments 2 and 3 of 'svsel'} } */
  svsel (pn, u8x2, s8x3); /* { dg-error {passing mismatched tuple types 'svuint8x2_t' and 'svint8x3_t' to arguments 2 and 3 of 'svsel'} } */
  svsel (pn, u8x2, u8x4); /* { dg-error {passing mismatched tuple types 'svuint8x2_t' and 'svuint8x4_t' to arguments 2 and 3 of 'svsel'} } */
  svsel (pn, s8x4, s8x2); /* { dg-error {passing mismatched tuple types 'svint8x4_t' and 'svint8x2_t' to arguments 2 and 3 of 'svsel'} } */
}

void
f2 (svcount_t pn, svuint8x2_t u8x2)
{
  svsel (pn, u8x2, u8x2); /* { dg-error {ACLE function 'svsel_u8_x2' can only be called when SME streaming mode is enabled} } */
}
