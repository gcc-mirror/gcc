/* { dg-do compile } */
/* { dg-additional-options "-Wall -Wextra" } */

#include <arm_sve.h>

void
f1 (svint32x4_t *ptr, svbool_t pg, svint32_t s32, svfloat64_t f64,
    svint32x4_t s32x4)
{
  *ptr = svcreate4 (s32); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&\)'} } */
  *ptr = svcreate4 (s32, s32); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&, svint32_t\&\)'} } */
  *ptr = svcreate4 (s32, s32, s32); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&, svint32_t\&, svint32_t\&\)'} } */
  *ptr = svcreate4 (s32, s32, s32, s32, s32); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&, svint32_t\&, svint32_t\&, svint32_t\&, svint32_t\&\)'} } */
  *ptr = svcreate4 (s32x4, s32x4, s32x4, s32x4); /* { dg-error {no matching function for call to 'svcreate4\(svint32x4_t\&, svint32x4_t\&, svint32x4_t\&, svint32x4_t\&\)'} } */
  *ptr = svcreate4 (s32, s32, s32, f64); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&, svint32_t\&, svint32_t\&, svfloat64_t\&\)'} } */
  *ptr = svcreate4 (s32, pg, s32, s32); /* { dg-error {no matching function for call to 'svcreate4\(svint32_t\&, svbool_t\&, svint32_t\&, svint32_t\&\)'} } */
  *ptr = svcreate4 (s32, s32, s32, s32);
  *ptr = svcreate4 (f64, f64, f64, f64); /* { dg-error {cannot convert 'svfloat64x4_t' to 'svint32x4_t' in assignment} } */
}
