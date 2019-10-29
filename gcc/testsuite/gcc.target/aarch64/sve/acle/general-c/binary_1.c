/* { dg-do compile } */

#include <arm_sve.h>

svuint8_t
f1 (svbool_t pg, svuint8_t u8, svint16_t s16)
{
  svzip1 (pg); /* { dg-error {too few arguments to function 'svzip1'} } */
  svzip1 (pg, u8, u8); /* { dg-error {too many arguments to function 'svzip1'} } */
  svzip1 (pg, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svzip1', but previous arguments had type 'svbool_t'} } */
  svzip1 (u8, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svzip1', but previous arguments had type 'svuint8_t'} } */
  svzip1 (u8, s16); /* { dg-error {passing 'svint16_t' to argument 2 of 'svzip1', but previous arguments had type 'svuint8_t'} } */
  svzip1 (u8, 0); /* { dg-error {passing 'int' to argument 2 of 'svzip1', which expects an SVE vector type} } */
}
