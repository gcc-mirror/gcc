/* Test for strftime formats.  Rejection of extensions in pedantic mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  /* %P is a lowercase version of %p.  */
  strftime (s, m, "%P", tp); /* { dg-warning "C" "strftime %P" } */
  /* %k is %H but padded with a space rather than 0 if necessary.  */
  strftime (s, m, "%k", tp); /* { dg-warning "C" "strftime %k" } */
  /* %l is %I but padded with a space rather than 0 if necessary.  */
  strftime (s, m, "%l", tp); /* { dg-warning "C" "strftime %l" } */
  /* %s is the number of seconds since the Epoch.  */
  strftime (s, m, "%s", tp); /* { dg-warning "C" "strftime %s" } */
  /* Extensions using %O already tested in c99-strftime-1.c.  */
  /* Width and flags are GNU extensions for strftime.  */
  strftime (s, m, "%20Y", tp); /* { dg-warning "C" "strftime width" } */
  strftime (s, m, "%^A", tp); /* { dg-warning "C" "strftime flags" } */
}
