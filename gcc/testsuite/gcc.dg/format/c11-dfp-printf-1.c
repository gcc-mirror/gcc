/* Test for printf formats: rejection of DFP formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-std=gnu11 -pedantic -Wformat" } */

#include "format.h"

void
foo (_Decimal32 d32, _Decimal64 d64, _Decimal128 d128) /* { dg-warning "ISO C" } */
{
  printf ("%Ha", d32); /* { dg-warning "C" } */
  printf ("%HA", d32); /* { dg-warning "C" } */
  printf ("%He", d32); /* { dg-warning "C" } */
  printf ("%HE", d32); /* { dg-warning "C" } */
  printf ("%Hf", d32); /* { dg-warning "C" } */
  printf ("%HF", d32); /* { dg-warning "C" } */
  printf ("%Hg", d32); /* { dg-warning "C" } */
  printf ("%HG", d32); /* { dg-warning "C" } */
  printf ("%Da", d64); /* { dg-warning "C" } */
  printf ("%DA", d64); /* { dg-warning "C" } */
  printf ("%De", d64); /* { dg-warning "C" } */
  printf ("%DE", d64); /* { dg-warning "C" } */
  printf ("%Df", d64); /* { dg-warning "C" } */
  printf ("%DF", d64); /* { dg-warning "C" } */
  printf ("%Dg", d64); /* { dg-warning "C" } */
  printf ("%DG", d64); /* { dg-warning "C" } */
  printf ("%DDa", d128); /* { dg-warning "C" } */
  printf ("%DDA", d128); /* { dg-warning "C" } */
  printf ("%DDe", d128); /* { dg-warning "C" } */
  printf ("%DDE", d128); /* { dg-warning "C" } */
  printf ("%DDf", d128); /* { dg-warning "C" } */
  printf ("%DDF", d128); /* { dg-warning "C" } */
  printf ("%DDg", d128); /* { dg-warning "C" } */
  printf ("%DDG", d128); /* { dg-warning "C" } */
}
