/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svuint8x2_t *ptr, svbool_t pg, svuint8_t u8, svfloat64_t f64,
    svuint8x2_t u8x2)
{
  *ptr = svcreate2 (u8); /* { dg-error {no matching function for call to 'svcreate2\(svuint8_t\&\)'} } */
  *ptr = svcreate2 (u8, u8, u8); /* { dg-error {no matching function for call to 'svcreate2\(svuint8_t\&, svuint8_t\&, svuint8_t\&\)'} } */
  *ptr = svcreate2 (u8x2, u8x2); /* { dg-error {no matching function for call to 'svcreate2\(svuint8x2_t\&, svuint8x2_t\&\)'} } */
  *ptr = svcreate2 (u8, f64); /* { dg-error {no matching function for call to 'svcreate2\(svuint8_t\&, svfloat64_t\&\)'} } */
  *ptr = svcreate2 (u8, pg); /* { dg-error {no matching function for call to 'svcreate2\(svuint8_t\&, svbool_t\&\)'} } */
  *ptr = svcreate2 (u8, u8);
  *ptr = svcreate2 (f64, f64); /* { dg-error {cannot convert 'svfloat64x2_t' to 'svuint8x2_t' in assignment} } */
}
