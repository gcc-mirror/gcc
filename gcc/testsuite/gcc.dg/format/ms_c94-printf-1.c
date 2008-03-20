/* Test for printf formats.  Changes in C94 to C90.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:199409 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (wint_t lc, wchar_t *ls)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.9.6.1 (pages 131-134),
     as amended by ISO/IEC 9899:1990/Amd.1:1995 (E) (pages 4-5).
     We do not repeat here all the C90 format checks, but just verify
     that %ls and %lc are accepted without warning.
  */
  printf ("%lc", lc);
  printf ("%ls", ls);
}
