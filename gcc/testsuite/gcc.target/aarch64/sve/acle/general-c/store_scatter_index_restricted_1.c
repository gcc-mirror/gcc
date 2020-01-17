/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct s { signed char x; };

svuint32_t
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    int32_t *s32_ptr, uint32_t *u32_ptr, float *f32_ptr,
    int64_t *s64_ptr, uint64_t *u64_ptr, double *f64_ptr,
    void *void_ptr, struct s *s_ptr, _Complex float *cf64_ptr,
    svint8_t s8, svuint8_t u8, svint16_t s16, svuint16_t u16, svfloat16_t f16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svstnt1_scatter_index (pg, s64_ptr, s64); /* { dg-error {too few arguments to function 'svstnt1_scatter_index'} } */
  svstnt1_scatter_index (pg, s64_ptr, s64, s64, 0); /* { dg-error {too many arguments to function 'svstnt1_scatter_index'} } */
  svstnt1_scatter_index (0, s64_ptr, s64, s64); /* { dg-error {passing 'int' to argument 1 of 'svstnt1_scatter_index', which expects 'svbool_t'} } */
  svstnt1_scatter_index (pg, 0, s64, s64);
  svstnt1_scatter_index (pg, (int64_t *) 0, s64, s64);
  svstnt1_scatter_index (pg, void_ptr, s64, s64);
  svstnt1_scatter_index (pg, s_ptr, s64, s64); /* { dg-warning "passing argument 2 of 'svstnt1_scatter_s64index_s64' from incompatible pointer type" } */
  svstnt1_scatter_index (pg, f32_ptr, s64, s64); /* { dg-warning "passing argument 2 of 'svstnt1_scatter_s64index_s64' from incompatible pointer type" } */
  svstnt1_scatter_index (pg, f64_ptr, s64, f64);
  svstnt1_scatter_index (pg, cf64_ptr, s64, f64); /* { dg-warning "passing argument 2 of 'svstnt1_scatter_s64index_f64' from incompatible pointer type" } */
  svstnt1_scatter_index (pg, s, s64, s64); /* { dg-error {passing 'struct s' to argument 2 of 'svstnt1_scatter_index', which expects a vector or pointer base address} } */

  svstnt1_scatter_index (pg, u32, void_ptr, s32); /* { dg-warning "passing argument 3 of 'svstnt1_scatter_u32base_index_s32' makes integer from pointer without a cast" } */
  svstnt1_scatter_index (pg, u32, pg, s32); /* { dg-error {passing 'svbool_t' to argument 3 of 'svstnt1_scatter_index', which expects 'int64_t'} } */
  svstnt1_scatter_index (pg, u32, s32, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svstnt1_scatter_index', which expects 'int64_t'} } */

  svstnt1_scatter_index (pg, void_ptr, u64, pg); /* { dg-error {passing 'svbool_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */

  svstnt1_scatter_index (pg, s8_ptr, u64, s8); /* { dg-error {passing 'svint8_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */
  svstnt1_scatter_index (pg, s8_ptr, u64, u8); /* { dg-error {passing 'svuint8_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */

  svstnt1_scatter_index (pg, s16_ptr, u64, s16); /* { dg-error {passing 'svint16_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */
  svstnt1_scatter_index (pg, s16_ptr, u64, u16); /* { dg-error {passing 'svuint16_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */
  svstnt1_scatter_index (pg, s16_ptr, u64, f16); /* { dg-error {passing 'svfloat16_t' to argument 4 of 'svstnt1_scatter_index', which expects a vector of 32-bit or 64-bit elements} } */

  svstnt1_scatter_index (pg, u32, 0, s32);
  svstnt1_scatter_index (pg, s32, 0, s32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint32_t'} } */

  svstnt1_scatter_index (pg, u32, 0, u32);
  svstnt1_scatter_index (pg, s32, 0, u32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint32_t'} } */

  svstnt1_scatter_index (pg, u32, 0, f32);
  svstnt1_scatter_index (pg, s32, 0, f32); /* { dg-error {passing 'svint32_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint32_t'} } */

  svstnt1_scatter_index (pg, u64, 0, s64);
  svstnt1_scatter_index (pg, s64, 0, s64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint64_t'} } */

  svstnt1_scatter_index (pg, u64, 0, u64);
  svstnt1_scatter_index (pg, s64, 0, u64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint64_t'} } */

  svstnt1_scatter_index (pg, u64, 0, f64);
  svstnt1_scatter_index (pg, s64, 0, f64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svstnt1_scatter_index', which expects 'svuint64_t'} } */

  svstnt1_scatter_index (pg, s32_ptr, s32, s32); /* { dg-error {when storing 'svint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, s32_ptr, u32, s32); /* { dg-error {when storing 'svint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, s32_ptr, f32, s32); /* { dg-error {when storing 'svint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */

  svstnt1_scatter_index (pg, u32_ptr, s32, u32); /* { dg-error {when storing 'svuint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, u32_ptr, u32, u32); /* { dg-error {when storing 'svuint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, u32_ptr, f32, u32); /* { dg-error {when storing 'svuint32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */

  svstnt1_scatter_index (pg, f32_ptr, s32, f32); /* { dg-error {when storing 'svfloat32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, f32_ptr, u32, f32); /* { dg-error {when storing 'svfloat32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */
  svstnt1_scatter_index (pg, f32_ptr, f32, f32);  /* { dg-error {when storing 'svfloat32_t', 'svstnt1_scatter_index' requires a vector base and a scalar index} } */

  svstnt1_scatter_index (pg, s64_ptr, s32, s64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, s64_ptr, u32, s64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, s64_ptr, f32, s64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, s64_ptr, s64, s64);
  svstnt1_scatter_index (pg, s64_ptr, u64, s64);
  svstnt1_scatter_index (pg, s64_ptr, f64, s64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svint64_t' expects a vector of 64-bit integers} } */

  svstnt1_scatter_index (pg, u64_ptr, s32, u64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, u64_ptr, u32, u64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, u64_ptr, f32, u64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, u64_ptr, s64, u64);
  svstnt1_scatter_index (pg, u64_ptr, u64, u64);
  svstnt1_scatter_index (pg, u64_ptr, f64, u64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svuint64_t' expects a vector of 64-bit integers} } */

  svstnt1_scatter_index (pg, f64_ptr, s32, f64); /* { dg-error {passing 'svint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, f64_ptr, u32, f64); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, f64_ptr, f32, f64); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
  svstnt1_scatter_index (pg, f64_ptr, s64, f64);
  svstnt1_scatter_index (pg, f64_ptr, u64, f64);
  svstnt1_scatter_index (pg, f64_ptr, f64, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svstnt1_scatter_index', which when storing 'svfloat64_t' expects a vector of 64-bit integers} } */
}
