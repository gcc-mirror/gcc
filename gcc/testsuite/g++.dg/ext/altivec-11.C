/* Test handling of literal constant for dss operation.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

void
foo ()
{
  vec_dss (1);
}
