/* Test for strfmon format checking.  Test for missing fill character
   at end of format.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m)
{
  strfmon (s, m, "%="); /* { dg-warning "missing fill character at end" } */
}
