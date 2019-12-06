/* { dg-do compile } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC target "+nosve"

void take_svuint8 (svuint8_t);

void
f (svuint8_t *ptr)
{
  take_svuint8 (*ptr); /* { dg-error {this operation requires the SVE ISA extension} } */
  /* { dg-error {'take_svuint8' requires the SVE ISA extension} "" { target *-*-* } .-1 } */
}
