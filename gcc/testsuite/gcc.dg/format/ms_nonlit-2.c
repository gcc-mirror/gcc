/* Test for warnings for non-string-literal formats.  Test with -Wformat=2.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat=2" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char *s, size_t i)
{
  printf ((const char *)i, i); /* { dg-warning "argument types" "non-literal" } */
  printf (s, i); /* { dg-warning "argument types" "non-literal" } */
}
