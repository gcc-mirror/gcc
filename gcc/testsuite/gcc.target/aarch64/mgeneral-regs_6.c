/* { dg-options "-O2 -march=armv8.2-a+sve" } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC push_options
#pragma GCC target "general-regs-only"

svint8x2_t
foo (svint8_t x0, svint8_t x1) /* { dg-error {'foo' requires the SVE ISA extension} } */
{
  return svcreate2 (x0, x1); /* { dg-error {ACLE function 'svcreate2_s8' is incompatible with the use of '-mgeneral-regs-only'} } */
}

#pragma GCC pop_options
