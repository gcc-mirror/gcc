/* Test the -Wno-format-zero-length option, which suppresses warnings
   about zero-length formats.  */
/* Origin: Jason Thorpe <thorpej@wasabisystems.com> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat -Wno-format-zero-length" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (void)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.9.6.1 (pages 131-134).  */
  /* Zero-length format strings are allowed.  */
  printf ("");
}
