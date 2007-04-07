/* Test diagnostics for suppressing contains nul
   -Wformat.  -Wformat.  */
/* Origin: Bruce Korb <bkorb@gnu.org> */
/* { dg-do compile } */
/* { dg-options "-Wformat -Wno-format-contains-nul" } */

#include "format.h"

void
foo (void)
{
  static char const fmt[] = "x%s\0%s\n\0abc";
  printf (fmt+4, fmt+8); /* { dg-bogus "embedded.*in format" "bogus embed warning" } */
}
