/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, int32_t *s32_ptr, enum svprfop op)
{
  svprfb (pg, s32_ptr, op); /* { dg-error {argument 3 of 'svprfb' must be an integer constant expression} } */
  svprfb (pg, s32_ptr, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 3 of 'svprfb', which expects a valid 'enum svprfop' value} } */
  svprfb (pg, s32_ptr, (enum svprfop) 0);
  svprfb (pg, s32_ptr, (enum svprfop) 5);
  svprfb (pg, s32_ptr, (enum svprfop) 6); /* { dg-error {passing 6 to argument 3 of 'svprfb', which expects a valid 'enum svprfop' value} } */
  svprfb (pg, s32_ptr, (enum svprfop) 7); /* { dg-error {passing 7 to argument 3 of 'svprfb', which expects a valid 'enum svprfop' value} } */
  svprfb (pg, s32_ptr, (enum svprfop) 8);
  svprfb (pg, s32_ptr, (enum svprfop) 14); /* { dg-error {passing 14 to argument 3 of 'svprfb', which expects a valid 'enum svprfop' value} } */
}
