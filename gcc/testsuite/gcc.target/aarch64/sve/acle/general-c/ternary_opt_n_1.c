/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svmla_x (pg, u8, u8); /* { dg-error {too few arguments to function 'svmla_x'} } */
  svmla_x (pg, u8, u8, u8, u8); /* { dg-error {too many arguments to function 'svmla_x'} } */
  svmla_x (u8, u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svmla_x', which expects 'svbool_t'} } */
  svmla_x (pg, pg, pg, pg); /* { dg-error {'svmla_x' has no form that takes 'svbool_t' arguments} } */
  svmla_x (pg, 1, u8, u8); /* { dg-error {passing 'int' to argument 2 of 'svmla_x', which expects an SVE vector type} } */
  svmla_x (pg, u8, s8, u8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, u8);
  svmla_x (pg, u8, s16, u8); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u16, u8); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, f16, u8); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, pg, u8); /* { dg-error {passing 'svbool_t' to argument 3 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, 0, u8); /* { dg-error {passing 'int' to argument 3 of 'svmla_x', which expects an SVE vector type} } */
  svmla_x (pg, u8, u8, s8); /* { dg-error {passing 'svint8_t' to argument 4 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, s16); /* { dg-error {passing 'svint16_t' to argument 4 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, u16); /* { dg-error {passing 'svuint16_t' to argument 4 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, f16); /* { dg-error {passing 'svfloat16_t' to argument 4 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, pg); /* { dg-error {passing 'svbool_t' to argument 4 of 'svmla_x', but previous arguments had type 'svuint8_t'} } */
  svmla_x (pg, u8, u8, 0);

  svmla_x (pg, f16, s16, f16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmla_x', but previous arguments had type 'svfloat16_t'} } */
  svmla_x (pg, f16, u16, f16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svmla_x', but previous arguments had type 'svfloat16_t'} } */
  svmla_x (pg, f16, f16, s16); /* { dg-error {passing 'svint16_t' to argument 4 of 'svmla_x', but previous arguments had type 'svfloat16_t'} } */
  svmla_x (pg, f16, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 4 of 'svmla_x', but previous arguments had type 'svfloat16_t'} } */
  svmla_x (pg, f16, f16, f16);
  svmla_x (pg, f16, f16, 1);
}
