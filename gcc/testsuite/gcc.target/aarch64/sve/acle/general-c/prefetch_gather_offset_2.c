/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svint8_t s8, svuint8_t u8,
    svint16_t s16, svuint16_t u16, svfloat16_t f16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, enum svprfop op)
{
  svprfb_gather (pg, u32); /* { dg-error {too few arguments to function 'svprfb_gather'} } */
  svprfb_gather (pg, u32, SV_PLDL1KEEP, 0); /* { dg-error {too many arguments to function 'svprfb_gather'} } */
  svprfb_gather (0, u32, SV_PLDL1KEEP); /* { dg-error {passing 'int' to argument 1 of 'svprfb_gather', which expects 'svbool_t'} } */
  svprfb_gather (pg, 0, SV_PLDL1KEEP); /* { dg-error {passing 'int' to argument 2 of 'svprfb_gather', which expects an SVE type rather than a scalar} } */

  svprfb_gather (pg, s8, SV_PLDL1KEEP); /* { dg-error {passing 'svint8_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, u8, SV_PLDL1KEEP); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, s16, SV_PLDL1KEEP); /* { dg-error {passing 'svint16_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, u16, SV_PLDL1KEEP); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, f16, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat16_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, s32, SV_PLDL1KEEP); /* { dg-error {passing 'svint32_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, u32, SV_PLDL1KEEP);
  svprfb_gather (pg, f32, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, s64, SV_PLDL1KEEP); /* { dg-error {passing 'svint64_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather (pg, u64, SV_PLDL1KEEP);
  svprfb_gather (pg, f64, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svprfb_gather', which expects 'svuint32_t' or 'svuint64_t'} } */

  svprfb_gather (pg, u32, op); /* { dg-error {argument 3 of 'svprfb_gather' must be an integer constant expression} } */
  svprfb_gather (pg, u32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 3 of 'svprfb_gather', which expects a valid 'enum svprfop' value} } */
  svprfb_gather (pg, u32, (enum svprfop) 0);
  svprfb_gather (pg, u32, (enum svprfop) 5);
  svprfb_gather (pg, u32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 3 of 'svprfb_gather', which expects a valid 'enum svprfop' value} } */
  svprfb_gather (pg, u32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 3 of 'svprfb_gather', which expects a valid 'enum svprfop' value} } */
  svprfb_gather (pg, u32, (enum svprfop) 8);
  svprfb_gather (pg, u32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 3 of 'svprfb_gather', which expects a valid 'enum svprfop' value} } */
}
