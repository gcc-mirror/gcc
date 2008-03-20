/* Test for scanf formats.  Formats using C99 features should be rejected
   outside of C99 mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (signed char *hhp, float *fp, llong *llp, intmax_t *jp,
     size_t *zp, ptrdiff_t *tp)
{
  /* Some tests already in c90-scanf-1.c.  */
  /* The widths hh, ll, j, z, t are new.  */
  scanf ("%hhd", hhp); /* { dg-warning "unknown|format" "%hh is unsupported" } */
  scanf ("%I64d", llp); /* { dg-warning "length|C" "%I64 in C90" } */
  scanf ("%jd", jp); /* { dg-warning "unknown|format" "%j is unsupported" } */
  scanf ("%zu", zp); /* { dg-warning "unknown|format" "%z is unsupported" } */
  scanf ("%td", tp); /* { dg-warning "unknown|format" "%t is unsupported" } */
  /* The formats F, a, A are new.  */
  scanf ("%F", fp); /* { dg-warning "unknown|format" "%F is unsupported" } */
  scanf ("%a", fp); /* { dg-warning "unknown|format" "%a is unsupported" } */
  scanf ("%A", fp); /* { dg-warning "unknown|format" "%A is unsupported" } */
}
