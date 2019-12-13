/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { int i; };

void
f1 (svbool_t pg, signed char *s8_ptr, short *s16_ptr,
    svint8_t s8, svint16_t s16,
    svint32_t s32, svuint32_t u32, svfloat32_t f32,
    svint64_t s64, svuint64_t u64, svfloat64_t f64, struct s s)
{
  svld1sb_gather_offset (pg, s8_ptr, s64); /* { dg-warning {implicit declaration of function 'svld1sb_gather_offset'; did you mean 'svld1_gather_offset'} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr); /* { dg-error {too few arguments to function 'svld1sb_gather_offset_u64'} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, s64, 0); /* { dg-error {too many arguments to function 'svld1sb_gather_offset_u64'} } */
  svld1sb_gather_offset_u64 (pg, s16_ptr, s64); /* { dg-warning {passing argument 2 of 'svld1sb_gather_s64offset_u64' from incompatible pointer type} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, pg); /* { dg-error {passing 'svbool_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, s16); /* { dg-error {passing 'svint16_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, s32); /* { dg-error {passing 'svint32_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, u32); /* { dg-error {passing 'svuint32_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, f32); /* { dg-error {passing 'svfloat32_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */
  svld1sb_gather_offset_u64 (pg, s8_ptr, s64);
  svld1sb_gather_offset_u64 (pg, s8_ptr, u64);
  svld1sb_gather_offset_u64 (pg, s8_ptr, f64); /* { dg-error {passing 'svfloat64_t' to argument 3 of 'svld1sb_gather_offset_u64', which expects a vector of 64-bit integers} } */

  svld1sb_gather_offset_u64 (pg, 0, s64);
  svld1sb_gather_offset_u64 (pg, s, s64); /* { dg-error {'struct s' to argument 2 of 'svld1sb_gather_offset_u64', which expects a vector or pointer base address} } */

  svld1sb_gather_offset_u64 (pg, pg, 0); /* { dg-error {passing 'svbool_t' to argument 2 of 'svld1sb_gather_offset_u64', which expects 'svuint64_t'} } */
  svld1sb_gather_offset_u64 (pg, s32, 0); /* { dg-error {passing 'svint32_t' to argument 2 of 'svld1sb_gather_offset_u64', which expects 'svuint64_t'} } */
  svld1sb_gather_offset_u64 (pg, u32, 0); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svld1sb_gather_offset_u64', which expects 'svuint64_t'} } */
  svld1sb_gather_offset_u64 (pg, u64, 0);
}
