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
  svst1q_scatter (pg, u64); /* { dg-error {too few arguments to function 'svst1q_scatter'} } */
  svst1q_scatter (pg, u64, s8, 0); /* { dg-error {too many arguments to function 'svst1q_scatter'} } */
  svst1q_scatter (0, u64, s8); /* { dg-error {passing 'int' to argument 1 of 'svst1q_scatter', which expects 'svbool_t'} } */
  svst1q_scatter (pg, 0, s32); /* { dg-error {passing 'int' to argument 2 of 'svst1q_scatter', which expects an SVE type rather than a scalar type} } */
  svst1q_scatter (pg, void_ptr, s32); /* { dg-error {passing 'void \*' to argument 2 of 'svst1q_scatter', which expects an SVE type rather than a scalar type} } */
  svst1q_scatter (pg, u32, s8); /* { dg-error {passing 'svuint32_t' to argument 2 of 'svst1q_scatter', which expects 'svuint64_t'} } */ 
  svst1q_scatter (pg, s32, s16); /* { dg-error {passing 'svint32_t' to argument 2 of 'svst1q_scatter', which expects 'svuint64_t'} } */
  svst1q_scatter (pg, u64, s32);
  svst1q_scatter (pg, s64, s64); /* { dg-error {passing 'svint64_t' to argument 2 of 'svst1q_scatter', which expects 'svuint64_t'} } */
  svst1q_scatter (pg, f64, u64); /* { dg-error {passing 'svfloat64_t' to argument 2 of 'svst1q_scatter', which expects 'svuint64_t'} } */

  svst1q_scatter (pg, u64, 0); /* { dg-error {passing 'int' to argument 3 of 'svst1q_scatter', which expects an SVE type rather than a scalar type} } */
  svst1q_scatter (pg, u64, pg); /* { dg-error {'svst1q_scatter' has no form that takes 'svbool_t' arguments} } */
  svst1q_scatter (pg, u64, s); /* { dg-error {passing 'struct s' to argument 3 of 'svst1q_scatter', which expects an SVE type} } */
  }
