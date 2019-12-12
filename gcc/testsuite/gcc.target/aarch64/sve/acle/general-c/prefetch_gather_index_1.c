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
  svprfh_gather_index (pg, s32_ptr, s32); /* { dg-error {too few arguments to function 'svprfh_gather_index'} } */
  svprfh_gather_index (pg, s32_ptr, s32, SV_PLDL1KEEP, 0); /* { dg-error {too many arguments to function 'svprfh_gather_index'} } */
  svprfh_gather_index (0, s32_ptr, s32, SV_PLDL1KEEP); /* { dg-error {passing 'int' to argument 1 of 'svprfh_gather_index', which expects 'svbool_t'} } */
  svprfh_gather_index (pg, 0, s32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, (int *) 0, s32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, void_ptr, s32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, ptr_ptr, s32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, s, s32, SV_PLDL1KEEP); /* { dg-error {passing 'struct s' to argument 2 of 'svprfh_gather_index', which expects a vector or pointer base address} } */

  svprfh_gather_index (pg, s32_ptr, s8, SV_PLDL1KEEP); /* { dg-error {passing 'svint8_t' to argument 3 of 'svprfh_gather_index', which expects a vector of 32-bit or 64-bit integers} } */
  svprfh_gather_index (pg, s32_ptr, u8, SV_PLDL1KEEP); /* { dg-error {passing 'svuint8_t' to argument 3 of 'svprfh_gather_index', which expects a vector of 32-bit or 64-bit integers} } */
  svprfh_gather_index (pg, s32_ptr, s16, SV_PLDL1KEEP); /* { dg-error {passing 'svint16_t' to argument 3 of 'svprfh_gather_index', which expects a vector of 32-bit or 64-bit integers} } */
  svprfh_gather_index (pg, s32_ptr, u16, SV_PLDL1KEEP); /* { dg-error {passing 'svuint16_t' to argument 3 of 'svprfh_gather_index', which expects a vector of 32-bit or 64-bit integers} } */
  svprfh_gather_index (pg, s32_ptr, f16, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat16_t' to argument 3 of 'svprfh_gather_index', which expects a vector of integers} } */
  svprfh_gather_index (pg, s32_ptr, s32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, s32_ptr, u32, SV_PLDL1KEEP);
  svprfh_gather_index (pg, s32_ptr, f32, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svprfh_gather_index', which expects a vector of integers} } */
  svprfh_gather_index (pg, s32_ptr, s64, SV_PLDL1KEEP);
  svprfh_gather_index (pg, s32_ptr, u64, SV_PLDL1KEEP);
  svprfh_gather_index (pg, s32_ptr, f64, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svprfh_gather_index', which expects a vector of integers} } */

  svprfh_gather_index (pg, u8, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svuint8_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfh_gather_index (pg, u16, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svuint16_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfh_gather_index (pg, s32, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svint32_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfh_gather_index (pg, u32, 0, SV_PLDL1KEEP);
  svprfh_gather_index (pg, f32, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat32_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfh_gather_index (pg, s64, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svint64_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */
  svprfh_gather_index (pg, u64, 0, SV_PLDL1KEEP);
  svprfh_gather_index (pg, f64, 0, SV_PLDL1KEEP); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svprfh_gather_index', which expects 'svuint32_t' or 'svuint64_t'} } */

  svprfh_gather_index (pg, s32_ptr, s32, op); /* { dg-error {argument 4 of 'svprfh_gather_index' must be an integer constant expression} } */
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 4 of 'svprfh_gather_index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 0);
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 5);
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 4 of 'svprfh_gather_index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 4 of 'svprfh_gather_index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 8);
  svprfh_gather_index (pg, s32_ptr, s32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 4 of 'svprfh_gather_index', which expects a valid 'enum svprfop' value} } */
}
