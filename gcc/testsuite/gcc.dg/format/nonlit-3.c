/* Test for warnings for non-string-literal formats.  Test for strftime formats.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wformat-nonliteral" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp, char *fmt)
{
  strftime (s, m, fmt, tp); /* { dg-warning "format string" "non-literal" } */
}
