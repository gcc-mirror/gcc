/* Test for strftime formats.  Rejection of extensions in pedantic mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  /* %P is a lowercase version of %p.  */
  strftime (s, m, "%P", tp); /* { dg-warning "unknown" "strftime %P" } */
  /* %k is %H but padded with a space rather than 0 if necessary.  */
  strftime (s, m, "%k", tp); /* { dg-warning "unknown" "strftime %k" } */
  /* %l is %I but padded with a space rather than 0 if necessary.  */
  strftime (s, m, "%l", tp); /* { dg-warning "unknown" "strftime %l" } */
  /* %s is the number of seconds since the Epoch.  */
  strftime (s, m, "%s", tp); /* { dg-warning "unknown" "strftime %s" } */
}
