/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, signed char *s8_ptr, void *void_ptr, struct s *s_ptr,
    float *f32_ptr, _Complex float *cf32_ptr)
{
  svld1_s8 (pg); /* { dg-error {too few arguments to function 'svld1_s8'} } */
  svld1_s8 (pg, s8_ptr, 0); /* { dg-error {too many arguments to function 'svld1_s8'} } */
  svld1_s8 (0, 0); /* { dg-error {incompatible type for argument 1 of 'svld1_s8'} } */
  svld1_s8 (pg, 0);
  svld1_s32 (pg, (int *) 0);
  svld1_s8 (pg, void_ptr);
  svld1_s8 (pg, s_ptr); /* { dg-warning {passing argument 2 of 'svld1_s8' from incompatible pointer type} } */
  svld1_f32 (pg, f32_ptr);
  svld1_f32 (pg, cf32_ptr); /* { dg-warning {passing argument 2 of 'svld1_f32' from incompatible pointer type} } */
  return svld1_s8 (pg, s8_ptr); /* { dg-error {incompatible types when returning type 'svint8_t' but 'svuint8_t' was expected} } */
}
