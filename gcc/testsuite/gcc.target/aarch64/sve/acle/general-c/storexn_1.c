/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

#pragma GCC target "+sme2"

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, svcount_t pn, svboolx2_t pgx2,
    signed char *s8_ptr, void *void_ptr, struct s *s_ptr,
    float *f32_ptr, _Complex float *cf32_ptr,
    svint8_t s8, svint8x2_t s8x2, svint8x3_t s8x3,
    svfloat32x4_t f32x4, struct s s) __arm_streaming
{
  svst1 (pn, s8_ptr); /* { dg-error {too few arguments to function 'svst1'} } */
  svst1 (pn, s8_ptr, s8x2, 0); /* { dg-error {too many arguments to function 'svst1'} } */
  svst1 (0, s8_ptr, s8x2); /* { dg-error {passing 'int' to argument 1 of 'svst1', which expects an 'svbool_t' or 'svcount_t'} } */
  svst1 (pn, void_ptr, 0x2); /* { dg-error {passing 'int' to argument 3 of 'svst1', which expects an SVE type rather than a scalar} } */
  svst1 (pn, void_ptr, pgx2); /* { dg-error {'svst1' has no form that takes 'svboolx2_t' arguments} } */
  svst1 (pn, 0, s8); /* { dg-error {operations on single vectors must be predicated by 'svbool_t' rather than 'svcount_t'} } */
  svst1 (pn, 0, s8x2);
  svst1 (pg, 0, s8x2); /* { dg-error {operations on multiple vectors must be predicated by 'svcount_t' rather than 'svbool_t'} } */
  svst1 (pn, 0, s8x3); /* { dg-error {'svst1' has no form that takes 'svint8x3_t' arguments} } */
  svst1 (pn, (int32_t *) 0, s8x2); /* { dg-error "passing argument 2 of 'svst1_s8_x2' from incompatible pointer type" } */
  svst1 (pn, void_ptr, s8x2);
  svst1 (pn, s_ptr, s8x2); /* { dg-error "passing argument 2 of 'svst1_s8_x2' from incompatible pointer type" } */
  svst1 (pn, f32_ptr, s8x2); /* { dg-error "passing argument 2 of 'svst1_s8_x2' from incompatible pointer type" } */
  svst1 (pn, f32_ptr, f32x4);
  svst1 (pn, cf32_ptr, f32x4); /* { dg-error "passing argument 2 of 'svst1_f32_x4' from incompatible pointer type" } */
  svst1 (pn, s, s8x2); /* { dg-error {passing 'struct s' to argument 2 of 'svst1', which expects a scalar pointer} } */
}
