/* Test for strftime formats.  Rejection of formats using C99 features in
   pedantic C90 mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat -Wformat-y2k" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%C", tp); /* { dg-warning "C" "%C not in C90" } */
  strftime (s, m, "%D", tp); /* { dg-warning "C" "%D not in C90" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 13 } */
  strftime (s, m, "%e", tp); /* { dg-warning "C" "%e not in C90" } */
  strftime (s, m, "%F", tp); /* { dg-warning "C" "%F not in C90" } */
  strftime (s, m, "%g", tp); /* { dg-warning "C" "%g not in C90" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } 17 } */
  strftime (s, m, "%G", tp); /* { dg-warning "C" "%G not in C90" } */
  strftime (s, m, "%h", tp); /* { dg-warning "C" "%h not in C90" } */
  strftime (s, m, "%n", tp); /* { dg-warning "C" "%n not in C90" } */
  strftime (s, m, "%r", tp); /* { dg-warning "C" "%r not in C90" } */
  strftime (s, m, "%R", tp); /* { dg-warning "C" "%R not in C90" } */
  strftime (s, m, "%t", tp); /* { dg-warning "C" "%t not in C90" } */
  strftime (s, m, "%T", tp); /* { dg-warning "C" "%T not in C90" } */
  strftime (s, m, "%u", tp); /* { dg-warning "C" "%u not in C90" } */
  strftime (s, m, "%V", tp); /* { dg-warning "C" "%V not in C90" } */
  strftime (s, m, "%z", tp); /* { dg-warning "C" "%z not in C90" } */
  strftime (s, m, "%EX", tp); /* { dg-warning "C" "%E not in C90" } */
  strftime (s, m, "%OW", tp); /* { dg-warning "C" "%O not in C90" } */
}
