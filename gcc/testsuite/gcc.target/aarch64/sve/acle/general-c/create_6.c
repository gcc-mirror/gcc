/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svint32x4_t *ptr, svbool_t pg, svint32_t s32, svfloat64_t f64,
    svint32x4_t s32x4, int x)
{
  *ptr = svcreate4_s32 (s32); /* { dg-error {too few arguments to function 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32, s32); /* { dg-error {too few arguments to function 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32, s32, s32); /* { dg-error {too few arguments to function 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32, s32, s32, s32, s32); /* { dg-error {too many arguments to function 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32x4, s32x4, s32x4, s32x4); /* { dg-error {incompatible type for argument 1 of 'svcreate4_s32'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate4_s32'} "" { target *-*-* } .-1 } */
  /* { dg-error {incompatible type for argument 3 of 'svcreate4_s32'} "" { target *-*-* } .-2 } */
  /* { dg-error {incompatible type for argument 4 of 'svcreate4_s32'} "" { target *-*-* } .-3 } */
  *ptr = svcreate4_s32 (s32, s32, s32, f64); /* { dg-error {incompatible type for argument 4 of 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32, s32, pg, s32); /* { dg-error {incompatible type for argument 3 of 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (s32, x, s32, s32); /* { dg-error {incompatible type for argument 2 of 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (x, s32, s32, s32); /* { dg-error {incompatible type for argument 1 of 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (pg, s32, s32, s32); /* { dg-error {incompatible type for argument 1 of 'svcreate4_s32'} } */
  *ptr = svcreate4_s32 (pg, pg, pg, pg); /* { dg-error {incompatible type for argument 1 of 'svcreate4_s32'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate4_s32'} "" { target *-*-* } .-1 } */
  /* { dg-error {incompatible type for argument 3 of 'svcreate4_s32'} "" { target *-*-* } .-2 } */
  /* { dg-error {incompatible type for argument 4 of 'svcreate4_s32'} "" { target *-*-* } .-3 } */
  *ptr = svcreate4_s32 (s32, s32, s32, s32);
  *ptr = svcreate4_f64 (f64, f64, f64, f64); /* { dg-error {incompatible types when assigning to type 'svint32x4_t' from type 'svfloat64x4_t'} } */
}
