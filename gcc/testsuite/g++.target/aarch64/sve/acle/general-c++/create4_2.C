/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svint32x4_t *ptr, svbool_t pg, svint32_t s32, svfloat64_t f64,
    svint32x4_t s32x4)
{
  *ptr = svcreate4_s32 (s32); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate4_s32 (s32, s32); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate4_s32 (s32, s32, s32); /* { dg-error {too few arguments to function '[^']*'} } */
  *ptr = svcreate4_s32 (s32, s32, s32, s32, s32); /* { dg-error {too many arguments to function '[^']*'} } */
  *ptr = svcreate4_s32 (s32x4, s32x4, s32x4, s32x4); /* { dg-error {cannot convert 'svint32x4_t' to 'svint32_t'} } */
  *ptr = svcreate4_s32 (s32, s32, s32, f64); /* { dg-error {cannot convert 'svfloat64_t' to 'svint32_t'} } */
  *ptr = svcreate4_s32 (s32, pg, s32, s32); /* { dg-error {cannot convert 'svbool_t' to 'svint32_t'} } */
  *ptr = svcreate4_s32 (s32, s32, s32, s32);
  *ptr = svcreate4_f64 (f64, f64, f64, f64); /* { dg-error {cannot convert 'svfloat64x4_t' to 'svint32x4_t' in assignment} } */
}
