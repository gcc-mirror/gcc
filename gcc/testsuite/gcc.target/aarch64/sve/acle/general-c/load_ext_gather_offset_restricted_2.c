/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct s { int i; };

void
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svldnt1sb_gather_offset (pg, s8_ptr, s32); /* { dg-error {implicit declaration of function 'svldnt1sb_gather_offset'; did you mean 'svldnt1_gather_offset'} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr); /* { dg-error {too few arguments to function 'svldnt1sb_gather_offset_u32'} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, u32, 0); /* { dg-error {too many arguments to function 'svldnt1sb_gather_offset_u32'} } */
  svldnt1sb_gather_offset_u32 (pg, s16_ptr, u32); /* { dg-error {passing argument 2 of 'svldnt1sb_gather_u32offset_u32' from incompatible pointer type} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, s32); /* { dg-error {'svldnt1sb_gather_offset_u32' does not support sign-extended offsets} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, u32);
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, u64); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */
  svldnt1sb_gather_offset_u32 (pg, s8_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1sb_gather_offset_u32', which expects a vector of 32-bit integers} } */

  svldnt1sb_gather_offset_u32 (pg, 0, u32);
  svldnt1sb_gather_offset_u32 (pg, s, u32); /* { dg-error {'struct s' to argument 2 of 'svldnt1sb_gather_offset_u32', which expects a vector or pointer base address} } */

  svldnt1sb_gather_offset_u32 (pg, pg, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svldnt1sb_gather_offset_u32', which expects 'svuint32_t'} } */
  svldnt1sb_gather_offset_u32 (pg, s32, 0); /* { dg-error {passing 'svint32_t' to argument 2 of 'svldnt1sb_gather_offset_u32', which expects 'svuint32_t'} } */
  svldnt1sb_gather_offset_u32 (pg, u32, 0);
  svldnt1sb_gather_offset_u32 (pg, u64, 0); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svldnt1sb_gather_offset_u32', which expects 'svuint32_t'} } */
}
