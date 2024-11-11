/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target ("+sve2p1")

struct s { signed char x; };

svuint32_t
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    int32_t *s32_ptr, float *f32_ptr, void *void_ptr, struct s *s_ptr,
    _Complex float *cf32_ptr,
    svint8_t s8, svint16_t s16, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svst1q_scatter_offset (pg, s32_ptr, u64); /* { dg-error {too few arguments to function 'svst1q_scatter_offset'} } */
  svst1q_scatter_offset (pg, s32_ptr, u64, s32, 0); /* { dg-error {too many arguments to function 'svst1q_scatter_offset'} } */
  svst1q_scatter_offset (0, s32_ptr, u64, s32); /* { dg-error {passing 'int' to argument 1 of 'svst1q_scatter_offset', which expects 'svbool_t'} } */
  svst1q_scatter_offset (pg, 0, u64, s32);
  svst1q_scatter_offset (pg, (int32_t *) 0, u64, s32);
  svst1q_scatter_offset (pg, void_ptr, u64, s32);
  svst1q_scatter_offset (pg, s_ptr, u64, s32); /* { dg-error "passing argument 2 of 'svst1q_scatter_u64offset_s32' from incompatible pointer type" } */
  svst1q_scatter_offset (pg, f32_ptr, u64, s32); /* { dg-error "passing argument 2 of 'svst1q_scatter_u64offset_s32' from incompatible pointer type" } */
  svst1q_scatter_offset (pg, f32_ptr, u64, f32);
  svst1q_scatter_offset (pg, cf32_ptr, u64, f32); /* { dg-error "passing argument 2 of 'svst1q_scatter_u64offset_f32' from incompatible pointer type" } */
  svst1q_scatter_offset (pg, s, u64, s32); /* { dg-error {passing 'struct s' to argument 2 of 'svst1q_scatter_offset', which expects a vector or pointer base address} } */

  svst1q_scatter_offset (pg, u64, void_ptr, s32); /* { dg-error "passing argument 3 of 'svst1q_scatter_u64base_offset_s32' makes integer from pointer without a cast" } */
  svst1q_scatter_offset (pg, u64, pg, s32); /* { dg-error {passing 'svbool_t' to argument 3 of 'svst1q_scatter_offset', which expects 'int64_t'} } */
  svst1q_scatter_offset (pg, u64, s64, s32); /* { dg-error {passing 'svint64_t' to argument 3 of 'svst1q_scatter_offset', which expects 'int64_t'} } */

  svst1q_scatter_offset (pg, void_ptr, u64, pg); /* { dg-error {'svst1q_scatter_offset' has no form that takes 'svbool_t' arguments} } */

  svst1q_scatter_offset (pg, u32, 0, s8); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */
  svst1q_scatter_offset (pg, s32, 0, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */

  svst1q_scatter_offset (pg, u32, 0, u16); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */
  svst1q_scatter_offset (pg, s32, 0, u32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */

  svst1q_scatter_offset (pg, u64, 0, s8);
  svst1q_scatter_offset (pg, s64, 0, s16); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */

  svst1q_scatter_offset (pg, u64, 0, u32);
  svst1q_scatter_offset (pg, s64, 0, u64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */

  svst1q_scatter_offset (pg, u64, 0, f64);
  svst1q_scatter_offset (pg, s64, 0, f64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1q_scatter_offset', which expects 'svuint64_t'} } */

  svst1q_scatter_offset (pg, s8_ptr, s8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svst1q_scatter_offset', which expects a vector of 64-bit integers} } */
  svst1q_scatter_offset (pg, s16_ptr, s16, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svst1q_scatter_offset', which expects a vector of 64-bit integers} } */
  svst1q_scatter_offset (pg, s32_ptr, s32, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svst1q_scatter_offset', which expects a vector of 64-bit integers} } */
  svst1q_scatter_offset (pg, f32_ptr, f32, s32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1q_scatter_offset', which expects a vector of 64-bit integers} } */
  svst1q_scatter_offset (pg, s32_ptr, s64, s32);
  svst1q_scatter_offset (pg, s32_ptr, u64, s32);
  svst1q_scatter_offset (pg, s32_ptr, f64, s32); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1q_scatter_offset', which expects a vector of 64-bit integers} } */
}
