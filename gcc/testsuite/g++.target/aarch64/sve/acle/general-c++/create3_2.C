/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svfloat16x3_t *ptr, svbool_t pg, svfloat16_t f16, svfloat64_t f64,
    svfloat16x3_t f16x3)
{
  *ptr = svcreate3_f16 (f16); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate3_f16 (f16, f16); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate3_f16 (f16, f16, f16, f16); /* { dg-error {too many arguments to function '[^']*'} } */
  *ptr = svcreate3_f16 (f16x3, f16x3, f16x3); /* { dg-error {cannot convert 'svfloat16x3_t' to 'svfloat16_t'} } */
  *ptr = svcreate3_f16 (f16, f16, f64); /* { dg-error {cannot convert 'svfloat64_t' to 'svfloat16_t'} } */
  *ptr = svcreate3_f16 (f16, pg, f16); /* { dg-error {cannot convert 'svbool_t' to 'svfloat16_t'} } */
  *ptr = svcreate3_f16 (f16, f16, f16);
  *ptr = svcreate3_f64 (f64, f64, f64); /* { dg-error {cannot convert 'svfloat64x3_t' to 'svfloat16x3_t' in assignment} } */
}
