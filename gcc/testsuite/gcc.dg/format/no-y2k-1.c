/* Test for warnings for Y2K problems not being on by default.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%y%c%x", tp);
}
