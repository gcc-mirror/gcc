/* Test for printf formats.  Formats using C99 features should be rejected
   outside of C99 mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#include "format.h"

void
foo (int i, double d, llong ll, intmax_t j, size_t z, ptrdiff_t t)
{
  /* Some tests already in c90-printf-1.c, e.g. %lf.  */
  /* The widths hh, ll, j, z, t are new.  */
  printf ("%hhd", i); /* { dg-warning "length|C" "%hh in C90" } */
  printf ("%lld", ll); /* { dg-warning "length|C" "%ll in C90" } */
  printf ("%jd", j); /* { dg-warning "length|C" "%j in C90" } */
  printf ("%zu", z); /* { dg-warning "length|C" "%z in C90" } */
  printf ("%td", t); /* { dg-warning "length|C" "%t in C90" } */
  /* The formats F, a, A are new.  */
  printf ("%F", d); /* { dg-warning "C" "%F in C90" } */
  printf ("%a", d); /* { dg-warning "C" "%a in C90" } */
  printf ("%A", d); /* { dg-warning "C" "%A in C90" } */
}
