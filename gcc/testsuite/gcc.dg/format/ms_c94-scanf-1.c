/* Test for scanf formats.  Changes in C94 to C90.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:199409 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (wchar_t *ls)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.9.6.2 (pages 134-138),
     as amended by ISO/IEC 9899:1990/Amd.1:1995 (E) (pages 5-6).
     We do not repeat here all the C90 format checks, but just verify
     that %ls, %lc, %l[] are accepted without warning.
  */
  scanf ("%lc%ls%l[abc]", ls, ls, ls);
}
