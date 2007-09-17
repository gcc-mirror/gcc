/* Test for scanf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#include "format.h"

void
foo (char **sp, wchar_t **lsp)
{
  /* m assignment-allocation modifier, recognized in both C90
     and C99 modes, is a POSIX and ISO/IEC WDTR 24731-2 extension.  */
  scanf ("%ms", sp); /* { dg-warning "C" "%ms" } */
  scanf ("%mS", lsp); /* { dg-warning "C" "%mS" } */
  scanf ("%mls", lsp); /* { dg-warning "C" "%mls" } */
  scanf ("%m[bcd]", sp); /* { dg-warning "C" "%m[]" } */
  scanf ("%ml[bcd]", lsp); /* { dg-warning "C" "%ml[]" } */
}
