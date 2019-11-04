/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint32_t
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    int32_t *s32_ptr, uint32_t *u32_ptr, float *f32_ptr,
    int64_t *s64_ptr, uint64_t *u64_ptr, double *f64_ptr,
    void *void_ptr, struct s *s_ptr, _Complex float *cf32_ptr,
    svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16, svfloat16_t f16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svst1_scatter_offset (pg, s32_ptr, s32); /* { dg-error {too few arguments to function 'svst1_scatter_offset'} } */
  svst1_scatter_offset (pg, s32_ptr, s32, s32, 0); /* { dg-error {too many arguments to function 'svst1_scatter_offset'} } */
  svst1_scatter_offset (0, s32_ptr, s32, s32); /* { dg-error {passing 'int' to argument 1 of 'svst1_scatter_offset', which expects 'svbool_t'} } */
  svst1_scatter_offset (pg, 0, s32, s32);
  svst1_scatter_offset (pg, (int *) 0, s32, s32);
  svst1_scatter_offset (pg, void_ptr, s32, s32);
  svst1_scatter_offset (pg, s_ptr, s32, s32); /* { dg-warning "passing argument 2 of 'svst1_scatter_s32offset_s32' from incompatible pointer type" } */
  svst1_scatter_offset (pg, f32_ptr, s32, s32); /* { dg-warning "passing argument 2 of 'svst1_scatter_s32offset_s32' from incompatible pointer type" } */
  svst1_scatter_offset (pg, f32_ptr, s32, f32);
  svst1_scatter_offset (pg, cf32_ptr, s32, f32); /* { dg-warning "passing argument 2 of 'svst1_scatter_s32offset_f32' from incompatible pointer type" } */
  svst1_scatter_offset (pg, s, s32, s32); /* { dg-error {passing 'struct s' to argument 2 of 'svst1_scatter_offset', which expects a vector or pointer base address} } */

  svst1_scatter_offset (pg, u32, void_ptr, s32); /* { dg-warning "passing argument 3 of 'svst1_scatter_u32base_offset_s32' makes integer from pointer without a cast" } */
  svst1_scatter_offset (pg, u32, pg, s32); /* { dg-error {passing 'svbool_t' to argument 3 of 'svst1_scatter_offset', which expects 'int64_t'} } */
  svst1_scatter_offset (pg, u32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svst1_scatter_offset', which expects 'int64_t'} } */

  svst1_scatter_offset (pg, void_ptr, u32, pg); /* { dg-error {passing 'svbool_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter_offset (pg, s8_ptr, u32, s8); /* { dg-error {passing 'svint8_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter_offset (pg, s8_ptr, u32, u8); /* { dg-error {passing 'svuint8_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter_offset (pg, s16_ptr, u32, s16); /* { dg-error {passing 'svint16_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter_offset (pg, s16_ptr, u32, u16); /* { dg-error {passing 'svuint16_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */
  svst1_scatter_offset (pg, s16_ptr, u32, f16); /* { dg-error {passing 'svfloat16_t' to argument 4 of 'svst1_scatter_offset', which expects a vector of 32-bit or 64-bit elements} } */

  svst1_scatter_offset (pg, u32, 0, s32);
  svst1_scatter_offset (pg, s32, 0, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint32_t'} } */

  svst1_scatter_offset (pg, u32, 0, u32);
  svst1_scatter_offset (pg, s32, 0, u32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint32_t'} } */

  svst1_scatter_offset (pg, u32, 0, f32);
  svst1_scatter_offset (pg, s32, 0, f32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint32_t'} } */

  svst1_scatter_offset (pg, u64, 0, s64);
  svst1_scatter_offset (pg, s64, 0, s64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint64_t'} } */

  svst1_scatter_offset (pg, u64, 0, u64);
  svst1_scatter_offset (pg, s64, 0, u64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint64_t'} } */

  svst1_scatter_offset (pg, u64, 0, f64);
  svst1_scatter_offset (pg, s64, 0, f64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1_scatter_offset', which expects 'svuint64_t'} } */

  svst1_scatter_offset (pg, s32_ptr, s32, s32);
  svst1_scatter_offset (pg, s32_ptr, u32, s32);
  svst1_scatter_offset (pg, s32_ptr, f32, s32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, s32_ptr, s64, s32); /* { dg-error {passing 'svint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, s32_ptr, u64, s32); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, s32_ptr, f64, s32); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint32_t' expects a vector of 32-bit integers} } */

  svst1_scatter_offset (pg, u32_ptr, s32, u32);
  svst1_scatter_offset (pg, u32_ptr, u32, u32);
  svst1_scatter_offset (pg, u32_ptr, f32, u32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, u32_ptr, s64, u32); /* { dg-error {passing 'svint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, u32_ptr, u64, u32); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, u32_ptr, f64, u32); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint32_t' expects a vector of 32-bit integers} } */

  svst1_scatter_offset (pg, f32_ptr, s32, f32);
  svst1_scatter_offset (pg, f32_ptr, u32, f32);
  svst1_scatter_offset (pg, f32_ptr, f32, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, f32_ptr, s64, f32); /* { dg-error {passing 'svint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, f32_ptr, u64, f32); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat32_t' expects a vector of 32-bit integers} } */
  svst1_scatter_offset (pg, f32_ptr, f64, f32); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat32_t' expects a vector of 32-bit integers} } */

  svst1_scatter_offset (pg, s64_ptr, s32, s64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, s64_ptr, u32, s64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, s64_ptr, f32, s64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, s64_ptr, s64, s64);
  svst1_scatter_offset (pg, s64_ptr, u64, s64);
  svst1_scatter_offset (pg, s64_ptr, f64, s64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svint64_t' expects a vector of 64-bit integers} } */

  svst1_scatter_offset (pg, u64_ptr, s32, u64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, u64_ptr, u32, u64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, u64_ptr, f32, u64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, u64_ptr, s64, u64);
  svst1_scatter_offset (pg, u64_ptr, u64, u64);
  svst1_scatter_offset (pg, u64_ptr, f64, u64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */

  svst1_scatter_offset (pg, f64_ptr, s32, f64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, f64_ptr, u32, f64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, f64_ptr, f32, f64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svst1_scatter_offset (pg, f64_ptr, s64, f64);
  svst1_scatter_offset (pg, f64_ptr, u64, f64);
  svst1_scatter_offset (pg, f64_ptr, f64, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svst1_scatter_offset', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
}
