/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { int i; };

void
f1 (svbool_t pg, int32_t *s32_ptr, void *void_ptr, void **ptr_ptr,
    svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16, svfloat16_t f16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, enum svprfop op,
    struct s s)
{
  svprfb_gather_offset (pg, s32_ptr, s32); /* { dg-error {too few arguments to function 'svprfb_gather_offset'} } */
  svprfb_gather_offset (pg, s32_ptr, s32, SV_PLDL1KEEP, 0); /* { dg-error {too many arguments to function 'svprfb_gather_offset'} } */
  svprfb_gather_offset (0, s32_ptr, s32, SV_PLDL1KEEP); /* { dg-error {passing 'int' to argument 1 of 'svprfb_gather_offset', which expects 'svbool_t'} } */
  svprfb_gather_offset (pg, 0, s32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, (int *) 0, s32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, void_ptr, s32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, ptr_ptr, s32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, s, s32, SV_PLDL1KEEP); /* { dg-error {passing 'struct s' to argument 2 of 'svprfb_gather_offset', which expects a vector or pointer base address} } */

  svprfb_gather_offset (pg, s32_ptr, s8, SV_PLDL1KEEP); /* { dg-error {passing 'svint8_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svprfb_gather_offset (pg, s32_ptr, u8, SV_PLDL1KEEP); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svprfb_gather_offset (pg, s32_ptr, s16, SV_PLDL1KEEP); /* { dg-error {passing 'svint16_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svprfb_gather_offset (pg, s32_ptr, u16, SV_PLDL1KEEP); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of 32-bit or 64-bit integers} } */
  svprfb_gather_offset (pg, s32_ptr, f16, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of integers} } */
  svprfb_gather_offset (pg, s32_ptr, s32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, s32_ptr, u32, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, s32_ptr, f32, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of integers} } */
  svprfb_gather_offset (pg, s32_ptr, s64, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, s32_ptr, u64, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, s32_ptr, f64, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svprfb_gather_offset', which expects a vector of integers} } */

  svprfb_gather_offset (pg, u8, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather_offset (pg, u16, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather_offset (pg, s32, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svint32_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather_offset (pg, u32, 0, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, f32, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather_offset (pg, s64, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svint64_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfb_gather_offset (pg, u64, 0, SV_PLDL1KEEP);
  svprfb_gather_offset (pg, f64, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svprfb_gather_offset', which expects 'svuint32_t' or 'svuint64_t'} } */

  svprfb_gather_offset (pg, s32_ptr, s32, op); /* { dg-error {argument 4 of 'svprfb_gather_offset' must be an integer constant expression} } */
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 4 of 'svprfb_gather_offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 0);
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 5);
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 4 of 'svprfb_gather_offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 4 of 'svprfb_gather_offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 8);
  svprfb_gather_offset (pg, s32_ptr, s32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 4 of 'svprfb_gather_offset', which expects a valid 'enum svprfop' value} } */
}
