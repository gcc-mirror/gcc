/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct s { signed char x; };

svuint64_t
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    int32_t *s32_ptr, uint32_t *u32_ptr, float *f32_ptr,
    int64_t *s64_ptr, uint64_t *u64_ptr, double *f64_ptr,
    void *void_ptr, struct s *s_ptr, _Complex float *cf32_ptr, int **ptr_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svldnt1_gather_index (pg, s64_ptr); /* { dg-error {too few arguments to function 'svldnt1_gather_index'} } */
  svldnt1_gather_index (pg, s64_ptr, s64, 0); /* { dg-error {too many arguments to function 'svldnt1_gather_index'} } */
  svldnt1_gather_index (0, s64_ptr, s64); /* { dg-error {passing 'int' to argument 1 of 'svldnt1_gather_index', which expects 'svbool_t'} } */
  svldnt1_gather_index (pg, 0, s64); /* { dg-error {passing 'int' to argument 2 of 'svldnt1_gather_index', which expects a pointer type} } */
  svldnt1_gather_index (pg, (uint64_t *) 0, s64);
  svldnt1_gather_index (pg, void_ptr, s64); /* { dg-error {passing 'void \*' to argument 2 of 'svldnt1_gather_index', but 'void' is not a valid SVE element type} } */
  svldnt1_gather_index (pg, s_ptr, s64); /* { dg-error {passing 'struct s \*' to argument 2 of 'svldnt1_gather_index', but 'struct s' is not a valid SVE element type} } */
  svldnt1_gather_index (pg, cf32_ptr, s32); /* { dg-error {passing '_Complex float \*' to argument 2 of 'svldnt1_gather_index', but 'complex float' is not a valid SVE element type} } */
  svldnt1_gather_index (pg, ptr_ptr, u64); /* { dg-error {passing 'int \*\*' to argument 2 of 'svldnt1_gather_index', but 'int \*' is not a valid SVE element type} } */
  svldnt1_gather_index (pg, u32, 0); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svldnt1_gather_index', which expects a pointer type} } */
  /* { dg-message {an explicit type suffix is needed when using a vector of base addresses} "" { target *-*-* } .-1 } */
  svldnt1_gather_index (pg, u64, 0); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svldnt1_gather_index', which expects a pointer type} } */
  /* { dg-message {an explicit type suffix is needed when using a vector of base addresses} "" { target *-*-* } .-1 } */

  svldnt1_gather_index (pg, s8_ptr, s8); /* { dg-error {passing 'signed char \*' to argument 2 of 'svldnt1_gather_index', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_index (pg, s8_ptr, s64); /* { dg-error {passing 'signed char \*' to argument 2 of 'svldnt1_gather_index', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_index (pg, s16_ptr, s16); /* { dg-error {passing 'short( int)? \*' to argument 2 of 'svldnt1_gather_index', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_index (pg, s16_ptr, s64); /* { dg-error {passing 'short( int)? \*' to argument 2 of 'svldnt1_gather_index', which expects a pointer to 32-bit or 64-bit elements} } */

  svldnt1_gather_index (pg, s32_ptr, s32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svint32_t'} } */
  svldnt1_gather_index (pg, s32_ptr, u32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svint32_t'} } */
  svldnt1_gather_index (pg, s32_ptr, f32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svint32_t'} } */
  svldnt1_gather_index (pg, s32_ptr, s64); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svint32_t'} } */

  svldnt1_gather_index (pg, u32_ptr, s32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svuint32_t'} } */
  svldnt1_gather_index (pg, u32_ptr, u32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svuint32_t'} } */
  svldnt1_gather_index (pg, u32_ptr, f32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svuint32_t'} } */
  svldnt1_gather_index (pg, u32_ptr, s64); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svuint32_t'} } */

  svldnt1_gather_index (pg, f32_ptr, s32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svfloat32_t'} } */
  svldnt1_gather_index (pg, f32_ptr, u32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svfloat32_t'} } */
  svldnt1_gather_index (pg, f32_ptr, f32); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svfloat32_t'} } */
  svldnt1_gather_index (pg, f32_ptr, s64); /* { dg-error {'svldnt1_gather_index' does not support 32-bit vector type 'svfloat32_t'} } */

  svldnt1_gather_index (pg, s64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, s64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, s64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, s64_ptr, s64);
  svldnt1_gather_index (pg, s64_ptr, u64);
  svldnt1_gather_index (pg, s64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svint64_t' expects a vector of 64-bit integers} } */

  svldnt1_gather_index (pg, u64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, u64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, u64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, u64_ptr, s64);
  svldnt1_gather_index (pg, u64_ptr, u64);
  svldnt1_gather_index (pg, u64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */

  svldnt1_gather_index (pg, f64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, f64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, f64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_index (pg, f64_ptr, s64);
  svldnt1_gather_index (pg, f64_ptr, u64);
  svldnt1_gather_index (pg, f64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_index', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */

  return svldnt1_gather_index (pg, s64_ptr, s64); /* { dg-error {incompatible types when returning type 'svint64_t' but 'svuint64_t' was expected} } */
}
