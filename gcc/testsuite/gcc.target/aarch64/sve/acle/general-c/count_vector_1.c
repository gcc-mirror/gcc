/* { dg-do compile } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint32_t u32, svuint32x2_t u32x2)
{
  svlen (); /* { dg-error {too few arguments to function 'svlen'} } */
  svlen (u32, u32); /* { dg-error {too many arguments to function 'svlen'} } */
  svlen (0); /* { dg-error {passing 'int' to argument 1 of 'svlen', which expects an SVE vector type} } */
  svlen (pg); /* { dg-error {'svlen' has no form that takes 'svbool_t' arguments} } */
  svlen (u32x2); /* { dg-error {passing 'svuint32x2_t' to argument 1 of 'svlen', which expects a single SVE vector rather than a tuple} } */
}
