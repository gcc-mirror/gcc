/* Test for printf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode. But allowed by -Wno-pedantic-ms-format.
*/
/* Origin: Kai Tietz <kai.tietz@onevision.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat -Wno-pedantic-ms-format" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i, long long ll, size_t z)
{
  printf ("%I32d", i);
  printf ("%I64x", ll);
  printf ("%Ix", z);
}
