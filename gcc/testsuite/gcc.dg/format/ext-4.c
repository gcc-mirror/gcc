/* Test for scanf formats.  %a extensions.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wformat" } */

#include "format.h"

void
foo (char **sp, wchar_t **lsp)
{
  /* %a formats for allocation, only recognised in C90 mode, are a
     GNU extension.  Followed by other characters, %a is not treated
     specially.
  */
  scanf ("%as", sp);
  scanf ("%aS", lsp);
  scanf ("%a[bcd]", sp);
}
