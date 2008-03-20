/* Test for scanf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (char **sp, wchar_t **lsp)
{
  /* m assignment-allocation modifier, recognized in both C90
     and C99 modes, is a POSIX and ISO/IEC WDTR 24731-2 extension.  */
  scanf ("%ms", sp); /* { dg-warning "unknown|format" "%ms is unsupported" } */
  scanf ("%mS", lsp); /* { dg-warning "unknown|format" "%mS is unsupported" } */
  scanf ("%mls", lsp); /* { dg-warning "unknown|format" "%mls is unsupported" } */
  scanf ("%m[bcd]", sp); /* { dg-warning "unknown|format" "%m[] is unsupported" } */
  scanf ("%ml[bcd]", lsp); /* { dg-warning "unknown|format" "%ml[] is unsupported" } */
}
