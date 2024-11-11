/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svint32x4_t *ptr, svbool_t pg, svint32_t s32, svfloat64_t f64,
    svint32x4_t s32x4, int x)
{
  *ptr = svcreate4 (s32); /* { dg-error {too few arguments to function 'svcreate4'} } */
  *ptr = svcreate4 (s32, s32); /* { dg-error {too few arguments to function 'svcreate4'} } */
  *ptr = svcreate4 (s32, s32, s32); /* { dg-error {too few arguments to function 'svcreate4'} } */
  *ptr = svcreate4 (s32, s32, s32, s32, s32); /* { dg-error {too many arguments to function 'svcreate4'} } */
  *ptr = svcreate4 (s32x4, s32x4, s32x4, s32x4); /* { dg-error {passing 'svint32x4_t' to argument 1 of 'svcreate4', which expects a single SVE vector rather than a tuple} } */
  *ptr = svcreate4 (s32, s32, s32, f64); /* { dg-error {passing 'svfloat64_t' to argument 4 of 'svcreate4', but argument 1 had type 'svint32_t'} } */
  *ptr = svcreate4 (s32, s32, pg, s32); /* { dg-error {passing 'svbool_t' to argument 3 of 'svcreate4', but argument 1 had type 'svint32_t'} } */
  *ptr = svcreate4 (s32, x, s32, s32); /* { dg-error {passing 'int' to argument 2 of 'svcreate4', which expects an SVE type rather than a scalar} } */
  *ptr = svcreate4 (x, s32, s32, s32); /* { dg-error {passing 'int' to argument 1 of 'svcreate4', which expects an SVE type rather than a scalar} } */
  *ptr = svcreate4 (pg, s32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svcreate4', but argument 1 had type 'svbool_t'} } */
  *ptr = svcreate4 (pg, pg, pg, pg); /* { dg-error {incompatible types when assigning to type 'svint32x4_t' from type 'svboolx4_t'} } */
  *ptr = svcreate4 (s32, s32, s32, s32);
  *ptr = svcreate4 (f64, f64, f64, f64); /* { dg-error {incompatible types when assigning to type 'svint32x4_t' from type 'svfloat64x4_t'} } */
}
