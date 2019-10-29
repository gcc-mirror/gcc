/* { dg-do compile } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC target "+nosve"

void f (svuint8_t x) {} /* { dg-error {'f' requires the SVE ISA extension} } */
