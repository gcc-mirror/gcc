/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv8.2-a+sve2")

struct s { signed char x; };

svuint32_t
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    int32_t *s32_ptr, uint32_t *u32_ptr, float *f32_ptr,
    int64_t *s64_ptr, uint64_t *u64_ptr, double *f64_ptr,
    void *void_ptr, struct s *s_ptr, _Complex float *cf32_ptr, int **ptr_ptr,
    svuint8_t u8, svuint16_t u16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64)
{
  svldnt1_gather_offset (pg, s32_ptr); /* { dg-error {too few arguments to function 'svldnt1_gather_offset'} } */
  svldnt1_gather_offset (pg, s32_ptr, s32, 0); /* { dg-error {too many arguments to function 'svldnt1_gather_offset'} } */
  svldnt1_gather_offset (0, s32_ptr, u32); /* { dg-error {passing 'int' to argument 1 of 'svldnt1_gather_offset', which expects 'svbool_t'} } */
  svldnt1_gather_offset (pg, 0, s32); /* { dg-error {passing 'int' to argument 2 of 'svldnt1_gather_offset', which expects a pointer type} } */
  svldnt1_gather_offset (pg, (int32_t *) 0, u32);
  svldnt1_gather_offset (pg, void_ptr, u32); /* { dg-error {passing 'void \*' to argument 2 of 'svldnt1_gather_offset', but 'void' is not a valid SVE element type} } */
  svldnt1_gather_offset (pg, s_ptr, u32); /* { dg-error {passing 'struct s \*' to argument 2 of 'svldnt1_gather_offset', but 'struct s' is not a valid SVE element type} } */
  svldnt1_gather_offset (pg, f32_ptr, u32);
  svldnt1_gather_offset (pg, cf32_ptr, u32); /* { dg-error {passing '_Complex float \*' to argument 2 of 'svldnt1_gather_offset', but 'complex float' is not a valid SVE element type} } */
  svldnt1_gather_offset (pg, ptr_ptr, u64); /* { dg-error {passing 'int \*\*' to argument 2 of 'svldnt1_gather_offset', but 'int \*' is not a valid SVE element type} } */
  svldnt1_gather_offset (pg, u32, 0); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svldnt1_gather_offset', which expects a pointer type} } */
  /* { dg-message {an explicit type suffix is needed when using a vector of base addresses} "" { target *-*-* } .-1 } */
  svldnt1_gather_offset (pg, u64, 0); /* { dg-error {passing 'svuint64_t' to argument 2 of 'svldnt1_gather_offset', which expects a pointer type} } */
  /* { dg-message {an explicit type suffix is needed when using a vector of base addresses} "" { target *-*-* } .-1 } */

  svldnt1_gather_offset (pg, s8_ptr, u8); /* { dg-error {passing 'signed char \*' to argument 2 of 'svldnt1_gather_offset', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_offset (pg, s8_ptr, u32); /* { dg-error {passing 'signed char \*' to argument 2 of 'svldnt1_gather_offset', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_offset (pg, s16_ptr, u16); /* { dg-error {passing 'short( int)? \*' to argument 2 of 'svldnt1_gather_offset', which expects a pointer to 32-bit or 64-bit elements} } */
  svldnt1_gather_offset (pg, s16_ptr, u32); /* { dg-error {passing 'short( int)? \*' to argument 2 of 'svldnt1_gather_offset', which expects a pointer to 32-bit or 64-bit elements} } */

  svldnt1_gather_offset (pg, s32_ptr, s32); /* { dg-error {'svldnt1_gather_offset' does not support 32-bit sign-extended offsets} } */
  svldnt1_gather_offset (pg, s32_ptr, u32);
  svldnt1_gather_offset (pg, s32_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, s32_ptr, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, s32_ptr, u64); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, s32_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint32_t' expects a vector of 32-bit integers} } */

  svldnt1_gather_offset (pg, u32_ptr, s32); /* { dg-error {'svldnt1_gather_offset' does not support 32-bit sign-extended offsets} } */
  svldnt1_gather_offset (pg, u32_ptr, u32);
  svldnt1_gather_offset (pg, u32_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, u32_ptr, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, u32_ptr, u64); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, u32_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint32_t' expects a vector of 32-bit integers} } */

  svldnt1_gather_offset (pg, f32_ptr, s32); /* { dg-error {'svldnt1_gather_offset' does not support 32-bit sign-extended offsets} } */
  svldnt1_gather_offset (pg, f32_ptr, u32);
  svldnt1_gather_offset (pg, f32_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, f32_ptr, s64); /* { dg-error {passing 'svint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, f32_ptr, u64); /* { dg-error {passing 'svuint64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat32_t' expects a vector of 32-bit integers} } */
  svldnt1_gather_offset (pg, f32_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat32_t' expects a vector of 32-bit integers} } */

  svldnt1_gather_offset (pg, s64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, s64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, s64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, s64_ptr, s64);
  svldnt1_gather_offset (pg, s64_ptr, u64);
  svldnt1_gather_offset (pg, s64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svint64_t' expects a vector of 64-bit integers} } */

  svldnt1_gather_offset (pg, u64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, u64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, u64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, u64_ptr, s64);
  svldnt1_gather_offset (pg, u64_ptr, u64);
  svldnt1_gather_offset (pg, u64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svuint64_t' expects a vector of 64-bit integers} } */

  svldnt1_gather_offset (pg, f64_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, f64_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, f64_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */
  svldnt1_gather_offset (pg, f64_ptr, s64);
  svldnt1_gather_offset (pg, f64_ptr, u64);
  svldnt1_gather_offset (pg, f64_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svldnt1_gather_offset', which when loading 'svfloat64_t' expects a vector of 64-bit integers} } */

  return svldnt1_gather_offset (pg, s32_ptr, u32); /* { dg-error {incompatible types when returning type 'svint32_t' but 'svuint32_t' was expected} } */
}
