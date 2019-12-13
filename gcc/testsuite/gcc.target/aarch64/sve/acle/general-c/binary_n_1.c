/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8, svfloat16_t f16, int i, float f)
{
  svinsr (u8); /* { dg-error {too few arguments to function 'svinsr'} } */
  svinsr (u8, 0, 0); /* { dg-error {too many arguments to function 'svinsr'} } */
  svinsr (0, 0); /* { dg-error {passing 'int' to argument 1 of 'svinsr', which expects an SVE vector type} } */
  svinsr (u8, 0);
  svinsr (u8, -1);
  svinsr (u8, i);
  svinsr (u8, f);
  svinsr (u8, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svinsr', which expects a scalar element} } */
  svinsr (pg, 0); /* { dg-error {'svinsr' has no form that takes 'svbool_t' arguments} } */
  svinsr (f16, f);
  svinsr (f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svinsr', which expects a scalar element} } */
}
