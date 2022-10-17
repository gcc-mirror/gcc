/* Test for scanf formats: rejection of DFP formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=gnu11 -pedantic -Wformat" } */

#include "format.h"

void
foo (_Decimal32 *d32, _Decimal64 *d64, _Decimal128 *d128) /* { dg-warning "ISO C" } */
{
  scanf ("%Ha", d32); /* { dg-warning "C" } */
  scanf ("%HA", d32); /* { dg-warning "C" } */
  scanf ("%He", d32); /* { dg-warning "C" } */
  scanf ("%HE", d32); /* { dg-warning "C" } */
  scanf ("%Hf", d32); /* { dg-warning "C" } */
  scanf ("%HF", d32); /* { dg-warning "C" } */
  scanf ("%Hg", d32); /* { dg-warning "C" } */
  scanf ("%HG", d32); /* { dg-warning "C" } */
  scanf ("%Da", d64); /* { dg-warning "C" } */
  scanf ("%DA", d64); /* { dg-warning "C" } */
  scanf ("%De", d64); /* { dg-warning "C" } */
  scanf ("%DE", d64); /* { dg-warning "C" } */
  scanf ("%Df", d64); /* { dg-warning "C" } */
  scanf ("%DF", d64); /* { dg-warning "C" } */
  scanf ("%Dg", d64); /* { dg-warning "C" } */
  scanf ("%DG", d64); /* { dg-warning "C" } */
  scanf ("%DDa", d128); /* { dg-warning "C" } */
  scanf ("%DDA", d128); /* { dg-warning "C" } */
  scanf ("%DDe", d128); /* { dg-warning "C" } */
  scanf ("%DDE", d128); /* { dg-warning "C" } */
  scanf ("%DDf", d128); /* { dg-warning "C" } */
  scanf ("%DDF", d128); /* { dg-warning "C" } */
  scanf ("%DDg", d128); /* { dg-warning "C" } */
  scanf ("%DDG", d128); /* { dg-warning "C" } */
}
