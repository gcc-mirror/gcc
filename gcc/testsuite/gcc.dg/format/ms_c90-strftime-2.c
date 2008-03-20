/* Test for strftime formats.  Rejection of formats using C99 features in
   pedantic C90 mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat -Wformat-y2k" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%C", tp); /* { dg-warning "format" "%C is unsupported" } */
  strftime (s, m, "%D", tp); /* { dg-warning "format" "%D is unsupported" } */
  strftime (s, m, "%e", tp); /* { dg-warning "format" "%e is unsupported" } */
  strftime (s, m, "%F", tp); /* { dg-warning "format" "%F is unsupported" } */
  strftime (s, m, "%g", tp); /* { dg-warning "format" "%g is unsupported" } */
  strftime (s, m, "%G", tp); /* { dg-warning "format" "%G is unsupported" } */
  strftime (s, m, "%h", tp); /* { dg-warning "format" "%h is unsupported" } */
  strftime (s, m, "%n", tp); /* { dg-warning "format" "%n is unsupported" } */
  strftime (s, m, "%r", tp); /* { dg-warning "format" "%r is unsupported" } */
  strftime (s, m, "%R", tp); /* { dg-warning "format" "%R is unsupported" } */
  strftime (s, m, "%t", tp); /* { dg-warning "format" "%t is unsupported" } */
  strftime (s, m, "%T", tp); /* { dg-warning "format" "%T is unsupported" } */
  strftime (s, m, "%u", tp); /* { dg-warning "format" "%u is unsupported" } */
  strftime (s, m, "%V", tp); /* { dg-warning "format" "%V is unsupported" } */
  strftime (s, m, "%z", tp); /* { dg-warning "C" "%z not in C90" } */
}
