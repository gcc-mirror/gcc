/* { dg-do compile } */
/* { dg-options "-march=armv8-a" } */
/* PR99381: we shouldn't ICE if the user forgets -march=armv8.2-a+sve. */

#include <arm_sve.h>
_Bool a;
int main()
{
  a = svaddv(svptrue_b8(), svdup_s8(0)); /* { dg-error "ACLE function 'svptrue_b8' requires ISA extension 'sve'" } */
}
