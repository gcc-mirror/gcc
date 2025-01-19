/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target "+nosve2p1"

void
f1 (svbool_t pg, int16_t *s16_ptr, svuint64_t u64)
{
  svld1q_gather_index (svptrue_b8 (), s16_ptr, u64); /* { dg-error {ACLE function 'svld1q_gather_u64index_s16' requires ISA extension 'sve2p1'} } */
}

#pragma GCC target "+sve2p1"

struct s { signed char x; };

svuint32_t
f2 (svbool_t pg, int8_t *s8_ptr, int16_t *s16_ptr,
    int32_t *s32_ptr, uint32_t *u32_ptr, float *f32_ptr,
    int64_t *s64_ptr, uint64_t *u64_ptr, double *f64_ptr,
    void *void_ptr, struct s *s_ptr, _Complex float *cf32_ptr, int **ptr_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svld1q_gather_index (pg, s32_ptr); /* { dg-error {too few arguments to function 'svld1q_gather_index'} } */
  svld1q_gather_index (pg, s32_ptr, s64, 0); /* { dg-error {too many arguments to function 'svld1q_gather_index'} } */
  svld1q_gather_index (0, s32_ptr, s64); /* { dg-error {passing 'int' to argument 1 of 'svld1q_gather_index', which expects 'svbool_t'} } */
  svld1q_gather_index (pg, 0, s64); /* { dg-error {passing 'int' to argument 2 of 'svld1q_gather_index', which expects a pointer type} } */
  svld1q_gather_index (pg, (int32_t *) 0, s64);
  svld1q_gather_index (pg, void_ptr, s64); /* { dg-error {passing 'void \*' to argument 2 of 'svld1q_gather_index', but 'void' is not a valid SVE element type} } */
  svld1q_gather_index (pg, s_ptr, s64); /* { dg-error {passing 'struct s \*' to argument 2 of 'svld1q_gather_index', but 'struct s' is not a valid SVE element type} } */
  svld1q_gather_index (pg, f32_ptr, s64);
  svld1q_gather_index (pg, cf32_ptr, s64); /* { dg-error {passing '_Complex float \*' to argument 2 of 'svld1q_gather_index', but 'complex float' is not a valid SVE element type} } */
  svld1q_gather_index (pg, ptr_ptr, u64); /* { dg-error {passing 'int \*\*' to argument 2 of 'svld1q_gather_index', but 'int \*' is not a valid SVE element type} } */
  svld1q_gather_index (pg, u32, 0); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svld1q_gather_index', which expects a pointer type} } */
  svld1q_gather_index (pg, u64, 0); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svld1q_gather_index', which expects a pointer type} } */

  svld1q_gather_index (pg, s8_ptr, s64); /* { dg-error {passing 'int8_t \*'[^\n]* to argument 2 of 'svld1q_gather_index', which expects the data to be 16 bits or wider} } */
  /* { dg-message {use the 'offset' rather than 'index' form for 8-bit data} "" { target *-*-* } .-1 } */

  svld1q_gather_index (pg, s32_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, s16_ptr, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, s32_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, u32_ptr, s64);

  svld1q_gather_index (pg, s32_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, s32_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, f32_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */
  svld1q_gather_index (pg, s32_ptr, s64);
  svld1q_gather_index (pg, u64_ptr, s64);
  svld1q_gather_index (pg, s64_ptr, u64);
  svld1q_gather_index (pg, f64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svld1q_gather_index', which expects a vector of 64-bit integers} } */

  return svld1q_gather_index (pg, s32_ptr, s64); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}
