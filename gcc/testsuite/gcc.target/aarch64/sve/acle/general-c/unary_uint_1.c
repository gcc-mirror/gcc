/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svexpa (); /* { dg-error {too few arguments to function 'svexpa'} } */
  svexpa (u16, u16); /* { dg-error {too many arguments to function 'svexpa'} } */
  svexpa (1); /* { dg-error {passing 'int' to argument 1 of 'svexpa', which expects an SVE vector type} } */
  svexpa (pg); /* { dg-error {passing 'svbool_t' to argument 1 of 'svexpa', which expects a vector of unsigned integers} } */
  svexpa (s8); /* { dg-error {passing 'svint8_t' to argument 1 of 'svexpa', which expects a vector of unsigned integers} } */
  svexpa (s16); /* { dg-error {passing 'svint16_t' to argument 1 of 'svexpa', which expects a vector of unsigned integers} } */
  svexpa (f16); /* { dg-error {passing 'svfloat16_t' to argument 1 of 'svexpa', which expects a vector of unsigned integers} } */

  svexpa (u8); /* { dg-error {'svexpa' has no form that takes 'svuint8_t' arguments} } */
  svexpa (u16);
}
