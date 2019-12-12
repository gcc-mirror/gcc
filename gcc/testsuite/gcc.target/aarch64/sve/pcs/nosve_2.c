/* { dg-do compile } */
/* { dg-prune-output "compilation terminated" } */

#include <arm_sve.h>

#pragma GCC target "+nosve"

svbool_t return_bool ();

void
f (svbool_t *ptr)
{
  *ptr = return_bool (); /* { dg-error {'return_bool' requires the SVE ISA extension} } */
}
