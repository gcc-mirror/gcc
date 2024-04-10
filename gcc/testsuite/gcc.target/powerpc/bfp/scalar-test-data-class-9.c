/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>
#include <stdbool.h>

bool
test_data_class (__ieee128 *p)
{
  __ieee128 source = *p;

  /* IEEE 128-bit floating point operations are only supported
     on 64-bit targets.  */
  return scalar_test_data_class (source, 256);	/* { dg-error "argument 2 must be a literal between 0 and 127, inclusive" } */
}

