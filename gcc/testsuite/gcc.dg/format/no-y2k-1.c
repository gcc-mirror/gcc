/* Test for warnings for Y2K problems being disabled by -Wno-format-y2k.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat -Wno-format-y2k" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%y%c%x", tp);
}
