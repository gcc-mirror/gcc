/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16)
{
  svmatch (pg, u8); /* { dg-error {too few arguments to function 'svmatch'} } */
  svmatch (pg, u8, u8, u8); /* { dg-error {too many arguments to function 'svmatch'} } */
  svmatch (u8, u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svmatch', which expects 'svbool_t'} } */
  svmatch (pg, pg, pg); /* { dg-error {'svmatch' has no form that takes 'svbool_t' arguments} } */
  svmatch (pg, 1, u8); /* { dg-error {passing 'int' to argument 2 of 'svmatch', which expects an SVE vector type} } */
  svmatch (pg, u8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svmatch', but previous arguments had type 'svuint8_t'} } */
  svmatch (pg, u8, u8);
  svmatch (pg, u8, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmatch', but previous arguments had type 'svuint8_t'} } */
  svmatch (pg, u8, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svmatch', but previous arguments had type 'svuint8_t'} } */
  svmatch (pg, u8, f16); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svmatch', but previous arguments had type 'svuint8_t'} } */
  svmatch (pg, u8, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svmatch', but previous arguments had type 'svuint8_t'} } */
  svmatch (pg, u8, 0); /* { dg-error {passing 'int' to argument 3 of 'svmatch', which expects an SVE vector type} } */

  svmatch (pg, f16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svmatch', but previous arguments had type 'svfloat16_t'} } */
  svmatch (pg, f16, u16); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svmatch', but previous arguments had type 'svfloat16_t'} } */
  svmatch (pg, f16, f16); /* { dg-error {'svmatch' has no form that takes 'svfloat16_t' arguments} } */
}
