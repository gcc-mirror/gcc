/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, signed char *s8_ptr, void *void_ptr, struct s *s_ptr,
    float *f32_ptr, _Complex float *cf32_ptr, svint8_t s8, svfloat32_t f32)
{
  svst1_vnum (pg, s8_ptr, 0); /* { dg-error {too few arguments to function 'svst1_vnum'} } */
  svst1_vnum (pg, s8_ptr, 0, s8, 0); /* { dg-error {too many arguments to function 'svst1_vnum'} } */
  svst1_vnum (0, s8_ptr, 0, s8); /* { dg-error {passing 'int' to argument 1 of 'svst1_vnum', which expects 'svbool_t'} } */
  svst1_vnum (pg, s8_ptr, pg, s8); /* { dg-error {passing 'svbool_t' to argument 3 of 'svst1_vnum', which expects 'int64_t'} } */
  svst1_vnum (pg, s8_ptr, s8, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svst1_vnum', which expects 'int64_t'} } */
  svst1_vnum (pg, s8_ptr, void_ptr, s8); /* { dg-warning "passing argument 3 of 'svst1_vnum_s8' makes integer from pointer without a cast" } */
  svst1_vnum (pg, void_ptr, 0, 0); /* { dg-error {passing 'int' to argument 4 of 'svst1_vnum', which expects an SVE vector type} } */
  svst1_vnum (pg, void_ptr, 0, pg); /* { dg-error {'svst1_vnum' has no form that takes 'svbool_t' arguments} } */
  svst1_vnum (pg, 0, 0, s8);
  svst1_vnum (pg, (int *) 0, 0, s8); /* { dg-warning "passing argument 2 of 'svst1_vnum_s8' from incompatible pointer type" } */
  svst1_vnum (pg, void_ptr, 0, s8);
  svst1_vnum (pg, s_ptr, 0, s8); /* { dg-warning "passing argument 2 of 'svst1_vnum_s8' from incompatible pointer type" } */
  svst1_vnum (pg, f32_ptr, 0, s8); /* { dg-warning "passing argument 2 of 'svst1_vnum_s8' from incompatible pointer type" } */
  svst1_vnum (pg, f32_ptr, 0, f32);
  svst1_vnum (pg, cf32_ptr, 0, f32); /* { dg-warning "passing argument 2 of 'svst1_vnum_f32' from incompatible pointer type" } */
}
