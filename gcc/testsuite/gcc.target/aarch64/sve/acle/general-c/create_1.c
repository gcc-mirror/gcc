/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svuint8x2_t *ptr, svbool_t pg, svuint8_t u8, svfloat64_t f64,
    svuint8x2_t u8x2, int x)
{
  *ptr = svcreate2 (u8); /* { dg-error {too few arguments to function 'svcreate2'} } */
  *ptr = svcreate2 (u8, u8, u8); /* { dg-error {too many arguments to function 'svcreate2'} } */
  *ptr = svcreate2 (u8x2, u8x2); /* { dg-error {passing 'svuint8x2_t' to argument 1 of 'svcreate2', which expects a single SVE vector rather than a tuple} } */
  *ptr = svcreate2 (u8, f64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svcreate2', but argument 1 had type 'svuint8_t'} } */
  *ptr = svcreate2 (u8, pg); /* { dg-error {passing 'svbool_t' to argument 2 of 'svcreate2', but argument 1 had type 'svuint8_t'} } */
  *ptr = svcreate2 (u8, x); /* { dg-error {passing 'int' to argument 2 of 'svcreate2', which expects an SVE type rather than a scalar} } */
  *ptr = svcreate2 (x, u8); /* { dg-error {passing 'int' to argument 1 of 'svcreate2', which expects an SVE type rather than a scalar} } */
  *ptr = svcreate2 (pg, u8); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svcreate2', but argument 1 had type 'svbool_t'} } */
  *ptr = svcreate2 (pg, pg); /* { dg-error {incompatible types when assigning to type 'svuint8x2_t' from type 'svboolx2_t'} } */
  *ptr = svcreate2 (u8, u8);
  *ptr = svcreate2 (f64, f64); /* { dg-error {incompatible types when assigning to type 'svuint8x2_t' from type 'svfloat64x2_t'} } */
}
