/* Test for warnings for non-string-literal formats.  Test for strftime formats.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-nonliteral" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp, char *fmt)
{
  strftime (s, m, fmt, tp); /* { dg-warning "format string" "non-literal" } */
}
