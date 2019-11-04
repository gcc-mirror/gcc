/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint8_t u8)
{
  svcnt_x (pg); /* { dg-error {too few arguments to function 'svcnt_x'} } */
  svcnt_x (pg, u8, u8); /* { dg-error {too many arguments to function 'svcnt_x'} } */
  svcnt_x (u8, u8); /* { dg-error {passing 'svuint8_t' to argument 1 of 'svcnt_x', which expects 'svbool_t'} } */
  svcnt_x (pg, pg); /* { dg-error {'svcnt_x' has no form that takes 'svbool_t' arguments} } */
  svcnt_x (pg, 1); /* { dg-error {passing 'int' to argument 2 of 'svcnt_x', which expects an SVE vector type} } */
  svcnt_x (pg, u8);
}
