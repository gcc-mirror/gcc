/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, signed char *s8_ptr, void *void_ptr, struct s *s_ptr,
    float *f32_ptr, _Complex float *cf32_ptr, int **ptr_ptr)
{
  svld1rq (pg); /* { dg-error {too few arguments to function 'svld1rq'} } */
  svld1rq (pg, s8_ptr, 0); /* { dg-error {too many arguments to function 'svld1rq'} } */
  svld1rq (0, s8_ptr); /* { dg-error {passing 'int' to argument 1 of 'svld1rq', which expects 'svbool_t'} } */
  svld1rq (pg, 0); /* { dg-error {passing 'int' to argument 2 of 'svld1rq', which expects a pointer type} } */
  svld1rq (pg, (int *) 0);
  svld1rq (pg, void_ptr); /* { dg-error {passing 'void \*' to argument 2 of 'svld1rq', but 'void' is not a valid SVE element type} } */
  svld1rq (pg, s_ptr); /* { dg-error {passing 'struct s \*' to argument 2 of 'svld1rq', but 'struct s' is not a valid SVE element type} } */
  svld1rq (pg, f32_ptr);
  svld1rq (pg, cf32_ptr); /* { dg-error {passing '_Complex float \*' to argument 2 of 'svld1rq', but 'complex float' is not a valid SVE element type} } */
  svld1rq (pg, ptr_ptr); /* { dg-error {passing 'int \*\*' to argument 2 of 'svld1rq', but 'int \*' is not a valid SVE element type} } */
  return svld1rq (pg, s8_ptr); /* { dg-error {incompatible types when returning type 'svint8_t' but 'svuint8_t' was expected} } */
}
