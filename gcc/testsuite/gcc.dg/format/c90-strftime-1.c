/* Test for strftime formats.  Formats using C90 features.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.12.3.5 (pages 174-175).  */
  /* Formats which are Y2K-compliant (no 2-digit years).  */
  strftime (s, m, "%a%A%b%B%d%H%I%j%m%M%p%S%U%w%W%X%Y%Z%%", tp);
  /* Formats with 2-digit years.  */
  strftime (s, m, "%y", tp); /* { dg-warning "only last 2" "2-digit year" } */
  /* Formats with 2-digit years in some locales.  */
  strftime (s, m, "%c", tp); /* { dg-warning "some locales" "2-digit year" } */
  strftime (s, m, "%x", tp); /* { dg-warning "some locales" "2-digit year" } */
}
