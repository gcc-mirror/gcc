/* { dg-do compile { target { powerpc*-*-* } } } */
/* Require 64-bit target to select expected error message below.  32-bit
   target produces different error message.  */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (__ieee128 *p, const int condition_flag)
{
  __ieee128 source = *p;

  return scalar_test_data_class (source, condition_flag); /* { dg-error "argument 2 must be a literal between 0 and 127, inclusive" } */
}

