/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svfloat16x3_t *ptr, svbool_t pg, svfloat16_t f16, svfloat64_t f64,
    svfloat16x3_t f16x3)
{
  *ptr = svcreate3 (f16); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16_t\&\)'} } */
  *ptr = svcreate3 (f16, f16); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16_t\&, svfloat16_t\&\)'} } */
  *ptr = svcreate3 (f16, f16, f16, f16); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16_t\&, svfloat16_t\&, svfloat16_t\&, svfloat16_t\&\)'} } */
  *ptr = svcreate3 (f16x3, f16x3, f16x3); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16x3_t\&, svfloat16x3_t\&, svfloat16x3_t\&\)'} } */
  *ptr = svcreate3 (f16, f16, f64); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16_t\&, svfloat16_t\&, svfloat64_t\&\)'} } */
  *ptr = svcreate3 (f16, pg, f16); /* { dg-error {no matching function for call to 'svcreate3\(svfloat16_t\&, svbool_t\&, svfloat16_t\&\)'} } */
  *ptr = svcreate3 (f16, f16, f16);
  *ptr = svcreate3 (f64, f64, f64); /* { dg-error {cannot convert 'svfloat64x3_t' to 'svfloat16x3_t' in assignment} } */
}
