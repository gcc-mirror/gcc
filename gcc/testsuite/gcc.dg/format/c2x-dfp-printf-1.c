/* Test for printf formats: acceptance of DFP formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=c2x -pedantic -Wformat" } */

#include "format.h"

void
foo (_Decimal32 d32, _Decimal64 d64, _Decimal128 d128)
{
  printf ("%Ha", d32);
  printf ("%HA", d32);
  printf ("%He", d32);
  printf ("%HE", d32);
  printf ("%Hf", d32);
  printf ("%HF", d32);
  printf ("%Hg", d32);
  printf ("%HG", d32);
  printf ("%Da", d64);
  printf ("%DA", d64);
  printf ("%De", d64);
  printf ("%DE", d64);
  printf ("%Df", d64);
  printf ("%DF", d64);
  printf ("%Dg", d64);
  printf ("%DG", d64);
  printf ("%DDa", d128);
  printf ("%DDA", d128);
  printf ("%DDe", d128);
  printf ("%DDE", d128);
  printf ("%DDf", d128);
  printf ("%DDF", d128);
  printf ("%DDg", d128);
  printf ("%DDG", d128);
}
