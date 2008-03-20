/* Test for warnings for Y2K problems not being on by default.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  strftime (s, m, "%y%c%x", tp);
}
