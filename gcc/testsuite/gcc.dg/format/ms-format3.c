/* Test for printf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode. But allowed by -Wno-pedantic-ms-format.
*/
/* Tests for specific MS types, origin: Ozkan Sezer <sezeroz@gmail.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat -Wno-pedantic-ms-format" } */

#define USE_SYSTEM_FORMATS
#define WIN32_LEAN_AND_MEAN
#include "format.h"
#include <windows.h>

void foo (LONG_PTR l, ULONG_PTR u, DWORD_PTR d, UINT_PTR p, SIZE_T s)
{
  printf ("%Id\n", l);
  printf ("%Iu\n", u);
  printf ("%Iu\n", d);
  printf ("%Iu\n", p);
  printf ("%Iu\n", s);
}
