/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svadd_x (pg, u8); /* { dg-error {too few arguments to function 'svadd_x'} } */
  svadd_x (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svadd_x'} } */
  svadd_x (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svadd_x', which expects 'svbool_t'} } */
  svadd_x (pg, pg, pg); /* { dg-error {'svadd_x' has no form that takes 'svbool_t' arguments} } */
  svadd_x (pg, 1, u8); /* { dg-error {passing 'int' to argument 2 of 'svadd_x', which expects an SVE vector type} } */
  svadd_x (pg, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svadd_x', but previous arguments had type 'svuint8_t'} } */
  svadd_x (pg, u8, u8);
  svadd_x (pg, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svadd_x', but previous arguments had type 'svuint8_t'} } */
  svadd_x (pg, u8, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svadd_x', but previous arguments had type 'svuint8_t'} } */
  svadd_x (pg, u8, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svadd_x', but previous arguments had type 'svuint8_t'} } */
  svadd_x (pg, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svadd_x', but previous arguments had type 'svuint8_t'} } */
  svadd_x (pg, u8, 0);

  svadd_x (pg, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svadd_x', but previous arguments had type 'svfloat16_t'} } */
  svadd_x (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svadd_x', but previous arguments had type 'svfloat16_t'} } */
  svadd_x (pg, f16, f16);
  svadd_x (pg, f16, 1);
}
