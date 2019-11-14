/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svuint8x2_t *ptr, svbool_t pg, svuint8_t u8, svfloat64_t f64,
    svuint8x2_t u8x2)
{
  *ptr = svcreate2_u8 (u8); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate2_u8 (u8, u8, u8); /* { dg-error {too many arguments to function '[^']*'} } */
  *ptr = svcreate2_u8 (u8x2, u8x2); /* { dg-error {cannot convert 'svuint8x2_t' to 'svuint8_t'} } */
  *ptr = svcreate2_u8 (u8, f64); /* { dg-error {cannot convert 'svfloat64_t' to 'svuint8_t'} } */
  *ptr = svcreate2_u8 (pg, u8); /* { dg-error {cannot convert 'svbool_t' to 'svuint8_t'} } */
  *ptr = svcreate2_u8 (u8, u8);
  *ptr = svcreate2_f64 (f64, f64); /* { dg-error {cannot convert 'svfloat64x2_t' to 'svuint8x2_t' in assignment} } */
}
