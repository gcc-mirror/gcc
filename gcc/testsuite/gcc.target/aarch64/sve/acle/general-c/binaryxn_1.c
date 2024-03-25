/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svcount_t pn, svuint8_t u8, svint16_t s16,
    svuint8x2_t u8x2, svuint8x3_t u8x3, svuint8x4_t u8x4)
{
  svsel (pg, u8); /* { dg-error {too few arguments to function 'svsel'} } */
  svsel (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svsel'} } */
  svsel (0, u8, u8); /* { dg-error {passing 'int' to argument 1 of 'svsel', which expects an 'svbool_t' or 'svcount_t'} } */
  svsel (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svsel', which expects an 'svbool_t' or 'svcount_t'} } */
  svsel (pn, u8, u8); /* { dg-error {operations on single vectors must be predicated by 'svbool_t' rather than 'svcount_t'} } */
  svsel (pg, pg, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svsel', but argument 2 had type 'svbool_t'} } */
  svsel (pg, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svsel', but argument 2 had type 'svuint8_t'} } */
  svsel (pg, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svsel', but argument 2 had type 'svuint8_t'} } */
  svsel (pg, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svsel', which expects an SVE type rather than a scalar} } */
  svsel (pg, pg, pg);
  svsel (pg, u8, u8);
  svsel (pg, u8, u8x2); /* { dg-error {passing tuple 'svuint8x2_t' to argument 3 of 'svsel' after passing single vector 'svuint8_t' to argument 2} } */
  svsel (pg, u8, u8x3); /* { dg-error {passing tuple 'svuint8x3_t' to argument 3 of 'svsel' after passing single vector 'svuint8_t' to argument 2} } */
  svsel (pg, u8, u8x4); /* { dg-error {passing tuple 'svuint8x4_t' to argument 3 of 'svsel' after passing single vector 'svuint8_t' to argument 2} } */
}
