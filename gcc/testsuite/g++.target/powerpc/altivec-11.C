/* Test handling of literal constant for dss operation.  */
/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

void
foo ()
{
  vec_dss (1);
}
