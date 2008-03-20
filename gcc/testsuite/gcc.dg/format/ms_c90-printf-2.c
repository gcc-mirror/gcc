/* Test for printf formats.  Formats using C99 features should be rejected
   outside of C99 mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i, double d, llong ll, intmax_t j, size_t z, ptrdiff_t t)
{
  /* Some tests already in c90-printf-1.c, e.g. %lf.  */
  /* The widths hh, ll, j, z, t are new.  */
  printf ("%hhd", i); /* { dg-warning "unknown|format" "%hh is unsupported" } */
  printf ("%I64d", ll); /* { dg-warning "length|C" "%I64 in C90" } */
  printf ("%jd", j); /* { dg-warning "unknown|format" "%j is unsupported" } */
  printf ("%zu", z); /* { dg-warning "unknown|format" "%z is unsupported" } */
  printf ("%td", t); /* { dg-warning "unknown|format" "%t is unsupported" } */
  /* The formats F, a, A are new.  */
  printf ("%F", d); /* { dg-warning "unknown|format" "%F is unsupported" } */
  printf ("%a", d); /* { dg-warning "unknown|format" "%a is unsupported" } */
  printf ("%A", d); /* { dg-warning "unknown|format" "%A is unsupported" } */
}
