/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svfloat16x3_t *ptr, svbool_t pg, svfloat16_t f16, svfloat64_t f64,
    svfloat16x3_t f16x3, int x)
{
  *ptr = svcreate3_f16 (f16); /* { dg-error {too few arguments to function 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (f16, f16); /* { dg-error {too few arguments to function 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (f16, f16, f16, f16); /* { dg-error {too many arguments to function 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (f16x3, f16x3, f16x3); /* { dg-error {incompatible type for argument 1 of 'svcreate3_f16'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate3_f16'} "" { target *-*-* } .-1 } */
  /* { dg-error {incompatible type for argument 3 of 'svcreate3_f16'} "" { target *-*-* } .-2 } */
  *ptr = svcreate3_f16 (f16, f16, f64); /* { dg-error {incompatible type for argument 3 of 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (f16, pg, f16); /* { dg-error {incompatible type for argument 2 of 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (f16, x, f16); /* { dg-error {incompatible type for argument 2 of 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (x, f16, f16); /* { dg-error {incompatible type for argument 1 of 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (pg, f16, f16); /* { dg-error {incompatible type for argument 1 of 'svcreate3_f16'} } */
  *ptr = svcreate3_f16 (pg, pg, pg); /* { dg-error {incompatible type for argument 1 of 'svcreate3_f16'} } */
  /* { dg-error {incompatible type for argument 2 of 'svcreate3_f16'} "" { target *-*-* } .-1 } */
  /* { dg-error {incompatible type for argument 3 of 'svcreate3_f16'} "" { target *-*-* } .-2 } */
  *ptr = svcreate3_f16 (f16, f16, f16);
  *ptr = svcreate3_f64 (f64, f64, f64); /* { dg-error {incompatible types when assigning to type 'svfloat16x3_t' from type 'svfloat64x3_t'} } */
}
