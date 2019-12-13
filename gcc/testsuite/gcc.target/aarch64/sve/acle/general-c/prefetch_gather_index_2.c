/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, int32_t *s32_ptr, svint32_t s32, enum svprfop op)
{
  svprfh_gather_s32index (pg, s32_ptr, s32, op); /* { dg-error {argument 4 of 'svprfh_gather_s32index' must be an integer constant expression} } */
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 4 of 'svprfh_gather_s32index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 0);
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 5);
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 4 of 'svprfh_gather_s32index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 4 of 'svprfh_gather_s32index', which expects a valid 'enum svprfop' value} } */
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 8);
  svprfh_gather_s32index (pg, s32_ptr, s32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 4 of 'svprfh_gather_s32index', which expects a valid 'enum svprfop' value} } */
}
