/* { dg-do compile } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC target "+nosve"

void unprototyped ();

void
f (svuint8_t *ptr)
{
  unprototyped (*ptr);  /* { dg-error {this operation requires the SVE ISA extension} } */
  /* { dg-error {arguments of type '(svuint8_t|__SVUint8_t)' require the SVE ISA extension} "" { target *-*-* } .-1 } */
}
