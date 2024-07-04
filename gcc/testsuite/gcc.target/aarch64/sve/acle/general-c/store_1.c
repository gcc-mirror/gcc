/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, signed char *s8_ptr, void *void_ptr, struct s *s_ptr,
    float *f32_ptr, _Complex float *cf32_ptr, svint8_t s8, svfloat32_t f32,
    struct s s)
{
  svst1 (pg, s8_ptr); /* { dg-error {too few arguments to function 'svst1'} } */
  svst1 (pg, s8_ptr, s8, 0); /* { dg-error {too many arguments to function 'svst1'} } */
  svst1 (0, s8_ptr, s8); /* { dg-error {passing 'int' to argument 1 of 'svst1', which expects an 'svbool_t' or 'svcount_t'} } */
  svst1 (pg, void_ptr, 0); /* { dg-error {passing 'int' to argument 3 of 'svst1', which expects an SVE type rather than a scalar} } */
  svst1 (pg, void_ptr, pg); /* { dg-error {'svst1' has no form that takes 'svbool_t' arguments} } */
  svst1 (pg, 0, s8);
  svst1 (pg, (int32_t *) 0, s8); /* { dg-error "passing argument 2 of 'svst1_s8' from incompatible pointer type" } */
  svst1 (pg, void_ptr, s8);
  svst1 (pg, s_ptr, s8); /* { dg-error "passing argument 2 of 'svst1_s8' from incompatible pointer type" } */
  svst1 (pg, f32_ptr, s8); /* { dg-error "passing argument 2 of 'svst1_s8' from incompatible pointer type" } */
  svst1 (pg, f32_ptr, f32);
  svst1 (pg, cf32_ptr, f32); /* { dg-error "passing argument 2 of 'svst1_f32' from incompatible pointer type" } */
  svst1 (pg, s, s8); /* { dg-error {passing 'struct s' to argument 2 of 'svst1', which expects a scalar pointer} } */
}
