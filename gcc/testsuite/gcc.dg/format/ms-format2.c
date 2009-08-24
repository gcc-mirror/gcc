/* Test for printf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode. But allowed by -Wno-pedantic-ms-format.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat -Wno-pedantic-ms-format" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

#ifdef _WIN64
#define XXX "%I64x"
#else
#define XXX "%I32x"
#endif

void
foo (float f, double d, void *p)
{
  printf (XXX, p); /* { dg-warning "format" "bad argument types" } */
  printf ("%I32x", f); /* { dg-warning "format" "bad argument types" } */
  printf ("%I64x", d); /* { dg-warning "format" "bad argument types" } */
}
