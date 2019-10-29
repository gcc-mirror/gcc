/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f1 (svbool_t pg, svuint32_t u32, enum svprfop op)
{
  svprfb_gather_u32base (pg, u32, op); /* { dg-error {argument 3 of 'svprfb_gather_u32base' must be an integer constant expression} } */
  svprfb_gather_u32base (pg, u32, (enum svprfop) -1); /* { dg-error {passing 4294967295 to argument 3 of 'svprfb_gather_u32base', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_u32base (pg, u32, (enum svprfop) 0);
  svprfb_gather_u32base (pg, u32, (enum svprfop) 5);
  svprfb_gather_u32base (pg, u32, (enum svprfop) 6); /* { dg-error {passing 6 to argument 3 of 'svprfb_gather_u32base', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_u32base (pg, u32, (enum svprfop) 7); /* { dg-error {passing 7 to argument 3 of 'svprfb_gather_u32base', which expects a valid 'enum svprfop' value} } */
  svprfb_gather_u32base (pg, u32, (enum svprfop) 8);
  svprfb_gather_u32base (pg, u32, (enum svprfop) 14); /* { dg-error {passing 14 to argument 3 of 'svprfb_gather_u32base', which expects a valid 'enum svprfop' value} } */
}
