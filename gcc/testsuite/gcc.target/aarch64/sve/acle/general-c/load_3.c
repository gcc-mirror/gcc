/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

struct s { signed char x; };

svuint8_t
f1 (svbool_t pg, signed char *s8_ptr, svint8_t s8)
{
  svld1_vnum (pg); /* { dg-error {too few arguments to function 'svld1_vnum'} } */
  svld1_vnum (pg, s8_ptr); /* { dg-error {too few arguments to function 'svld1_vnum'} } */
  svld1_vnum (pg, s8_ptr, 0, 0); /* { dg-error {too many arguments to function 'svld1_vnum'} } */
  svld1_vnum (0, s8_ptr, 0); /* { dg-error {passing 'int' to argument 1 of 'svld1_vnum', which expects 'svbool_t'} } */
  svld1_vnum (pg, 0, 0); /* { dg-error {passing 'int' to argument 2 of 'svld1_vnum', which expects a pointer type} } */
  svld1_vnum (pg, s8_ptr, s8_ptr); /* { dg-error "passing argument 3 of 'svld1_vnum_s8' makes integer from pointer without a cast" } */
  svld1_vnum (pg, s8_ptr, s8); /* { dg-error {passing 'svint8_t' to argument 3 of 'svld1_vnum', which expects 'int64_t'} } */
}
