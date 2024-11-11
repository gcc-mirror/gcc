/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svint32_t s32)
{
  svpmov (s32); /* { dg-error {ACLE function 'svpmov_s32' requires ISA extension 'sve2p1'} } */
}

#pragma GCC target "+sve2p1"

void
f2 (svbool_t pg, svint8_t s8, svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svuint32x2_t u32x2)
{
  svpmov (); /* { dg-error {too few arguments to function 'svpmov'} } */
  svpmov (s8, s8); /* { dg-error {too many arguments to function 'svpmov'} } */

  svpmov (0); /* { dg-error {passing 'int' to argument 1 of 'svpmov', which expects an SVE type rather than a scalar type} } */
  svpmov (pg); /* { dg-error {'svpmov' has no form that takes 'svbool_t' arguments} } */
  svpmov (s32);
  svpmov (u32);
  svpmov (f32); /* { dg-error {'svpmov' has no form that takes 'svfloat32_t' arguments} } */
  svpmov (u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 1 of 'svpmov', which expects a single SVE vector rather than a tuple} } */
}
