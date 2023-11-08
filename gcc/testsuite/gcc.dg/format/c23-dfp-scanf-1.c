/* Test for scanf formats: acceptance of DFP formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=c23 -pedantic -Wformat" } */

#include "format.h"

void
foo (_Decimal32 *d32, _Decimal64 *d64, _Decimal128 *d128)
{
  scanf ("%Ha", d32);
  scanf ("%HA", d32);
  scanf ("%He", d32);
  scanf ("%HE", d32);
  scanf ("%Hf", d32);
  scanf ("%HF", d32);
  scanf ("%Hg", d32);
  scanf ("%HG", d32);
  scanf ("%Da", d64);
  scanf ("%DA", d64);
  scanf ("%De", d64);
  scanf ("%DE", d64);
  scanf ("%Df", d64);
  scanf ("%DF", d64);
  scanf ("%Dg", d64);
  scanf ("%DG", d64);
  scanf ("%DDa", d128);
  scanf ("%DDA", d128);
  scanf ("%DDe", d128);
  scanf ("%DDE", d128);
  scanf ("%DDf", d128);
  scanf ("%DDF", d128);
  scanf ("%DDg", d128);
  scanf ("%DDG", d128);
}
