/* Test handling of literal constant for dss operation.  */
/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

void
foo ()
{
  vec_dss (1);
}
