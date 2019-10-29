/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svcmpeq (pg, u8); /* { dg-error {too few arguments to function 'svcmpeq'} } */
  svcmpeq (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svcmpeq'} } */
  svcmpeq (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svcmpeq', which expects 'svbool_t'} } */
  svcmpeq (pg, pg, pg); /* { dg-error {'svcmpeq' has no form that takes 'svbool_t' arguments} } */
  svcmpeq (pg, 1, u8); /* { dg-error {passing 'int' to argument 2 of 'svcmpeq', which expects an SVE vector type} } */
  svcmpeq (pg, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svuint8_t'} } */
  svcmpeq (pg, u8, u8);
  svcmpeq (pg, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svuint8_t'} } */
  svcmpeq (pg, u8, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svuint8_t'} } */
  svcmpeq (pg, u8, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svuint8_t'} } */
  svcmpeq (pg, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svuint8_t'} } */
  svcmpeq (pg, u8, 0);

  svcmpeq (pg, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svfloat16_t'} } */
  svcmpeq (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svcmpeq', but previous arguments had type 'svfloat16_t'} } */
  svcmpeq (pg, f16, f16);
  svcmpeq (pg, f16, 1);
}
