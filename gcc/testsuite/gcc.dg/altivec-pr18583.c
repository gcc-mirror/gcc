/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-ibm-aix*" } { "-maltivec" } { "" } } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

static const vector signed short c[1] = 
  {(const vector signed short){4095, 5681, 5351, 4816, 4095, 4816, 5351, 5681}};
