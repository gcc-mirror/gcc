/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, int32_t *s32_ptr, svint32_t s32, enum svprfop op)
{
  svprfb_gather_s32offset (pg, s32_ptr, s32, op); /* { dg-error {argument 4 of 'svprfb_gather_s32offset' must be an integer constant expression} } */
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 4 of 'svprfb_gather_s32offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 0);
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 5);
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 4 of 'svprfb_gather_s32offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 4 of 'svprfb_gather_s32offset', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 8);
  svprfb_gather_s32offset (pg, s32_ptr, s32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 4 of 'svprfb_gather_s32offset', which expects a valid 'enum svprfop' value} } */
}
