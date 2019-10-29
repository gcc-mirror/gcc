/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svfloat16x3_t *ptr, svbool_t pg, svfloat16_t f16, svfloat64_t f64,
    svfloat16x3_t f16x3, int x)
{
  *ptr = svcreate3 (f16); /* { dg-error {too few arguments to function 'svcreate3'} } */
  *ptr = svcreate3 (f16, f16); /* { dg-error {too few arguments to function 'svcreate3'} } */
  *ptr = svcreate3 (f16, f16, f16, f16); /* { dg-error {too many arguments to function 'svcreate3'} } */
  *ptr = svcreate3 (f16x3, f16x3, f16x3); /* { dg-error {passing 'svfloat16x3_t' to argument 1 of 'svcreate3', which expects a single SVE vector rather than a tuple} } */
  *ptr = svcreate3 (f16, f16, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svcreate3', but previous arguments had type 'svfloat16_t'} } */
  *ptr = svcreate3 (f16, pg, f16); /* { dg-error {passing 'svbool_t' to argument 2 of 'svcreate3', but previous arguments had type 'svfloat16_t'} } */
  *ptr = svcreate3 (f16, x, f16); /* { dg-error {passing 'int' to argument 2 of 'svcreate3', which expects an SVE vector type} } */
  *ptr = svcreate3 (x, f16, f16); /* { dg-error {passing 'int' to argument 1 of 'svcreate3', which expects an SVE vector type} } */
  *ptr = svcreate3 (pg, f16, f16); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svcreate3', but previous arguments had type 'svbool_t'} } */
  *ptr = svcreate3 (pg, pg, pg); /* { dg-error {'svcreate3' has no form that takes 'svbool_t' arguments} } */
  *ptr = svcreate3 (f16, f16, f16);
  *ptr = svcreate3 (f64, f64, f64); /* { dg-error {incompatible types when assigning to type 'svfloat16x3_t' from type 'svfloat64x3_t'} } */
}
