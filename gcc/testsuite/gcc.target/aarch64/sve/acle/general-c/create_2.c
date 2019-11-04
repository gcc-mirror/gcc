/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svuint8x2_t *ptr, svbool_t pg, svuint8_t u8, svfloat64_t f64,
    svuint8x2_t u8x2, int x)
{
  *ptr = svcreate2_u8 (u8); /* { dg-error {too few arguments to function 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (u8, u8, u8); /* { dg-error {too many arguments to function 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (u8x2, u8x2); /* { dg-error {incompatible type for argument 1 of 'svcreate2_u8'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate2_u8'} "" { target *-*-* } .-1 } */
  *ptr = svcreate2_u8 (u8, f64); /* { dg-error {incompatible type for argument 2 of 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (u8, pg); /* { dg-error {incompatible type for argument 2 of 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (u8, x); /* { dg-error {incompatible type for argument 2 of 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (x, u8); /* { dg-error {incompatible type for argument 1 of 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (pg, u8); /* { dg-error {incompatible type for argument 1 of 'svcreate2_u8'} } */
  *ptr = svcreate2_u8 (pg, pg); /* { dg-error {incompatible type for argument 1 of 'svcreate2_u8'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate2_u8'} "" { target *-*-* } .-1 } */
  *ptr = svcreate2_u8 (u8, u8);
  *ptr = svcreate2_f64 (f64, f64); /* { dg-error {incompatible types when assigning to type 'svuint8x2_t' from type 'svfloat64x2_t'} } */
}
