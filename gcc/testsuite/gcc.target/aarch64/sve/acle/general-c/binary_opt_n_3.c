/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svand_z (pg, u8); /* { dg-error {too few arguments to function 'svand_z'} } */
  svand_z (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svand_z'} } */
  svand_z (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svand_z', which expects 'svbool_t'} } */
  svand_z (pg, pg, pg);
  svand_z (pg, 1, u8); /* { dg-error {passing 'int' to argument 2 of 'svand_z', which expects an SVE vector type} } */
  svand_z (pg, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svand_z', but previous arguments had type 'svuint8_t'} } */
  svand_z (pg, u8, u8);
  svand_z (pg, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svand_z', but previous arguments had type 'svuint8_t'} } */
  svand_z (pg, u8, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svand_z', but previous arguments had type 'svuint8_t'} } */
  svand_z (pg, u8, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svand_z', but previous arguments had type 'svuint8_t'} } */
  svand_z (pg, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svand_z', but previous arguments had type 'svuint8_t'} } */
  svand_z (pg, u8, 0);

  svand_z (pg, pg, u8); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svand_z', but previous arguments had type 'svbool_t'} } */
  svand_z (pg, pg, 0); /* { dg-error {passing 'int' to argument 3 of 'svand_z', but its 'svbool_t' form does not accept scalars} } */

  svand_z (pg, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svand_z', but previous arguments had type 'svfloat16_t'} } */
  svand_z (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svand_z', but previous arguments had type 'svfloat16_t'} } */
  svand_z (pg, f16, f16); /* { dg-error {'svand_z' has no form that takes 'svfloat16_t' arguments} } */
  svand_z (pg, f16, 1); /* { dg-error {'svand_z' has no form that takes 'svfloat16_t' arguments} } */
}
