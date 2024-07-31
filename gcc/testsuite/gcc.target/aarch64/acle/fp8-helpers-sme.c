/* Test availability of the fp8 ACLE helper functions when including arm_sme.h.
 */
/* { dg-do compile } */
/* { dg-options "-std=c90 -pedantic-errors -O1 -march=armv8-a" } */

#include <arm_sme.h>

void
test_fpmr_helpers_present ()
{
  (__arm_fpm_init ());
}
