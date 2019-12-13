/* { dg-do compile } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC target "+nosve"

void
f (float a, float b, float c, float d, float e, float f, float g, float h, svuint8_t x) /* { dg-error {arguments of type '(svuint8_t|__SVUint8_t)' require the SVE ISA extension} } */
{
}
