/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8)
{
  svabs_x (pg); /* { dg-error {too few arguments to function 'svabs_x'} } */
  svabs_x (pg, s8, s8); /* { dg-error {too many arguments to function 'svabs_x'} } */
  svabs_x (s8, s8); /* { dg-error {passing 'svint8_t' to argument 1 of 'svabs_x', which expects 'svbool_t'} } */
  svabs_x (pg, pg); /* { dg-error {'svabs_x' has no form that takes 'svbool_t' arguments} } */
  svabs_x (pg, 1); /* { dg-error {passing 'int' to argument 2 of 'svabs_x', which expects an SVE vector type} } */
  svabs_x (pg, s8);
  svabs_x (pg, u8); /* { dg-error {'svabs_x' has no form that takes 'svuint8_t' arguments} } */
}
