/* Test for format diagnostics.  Proper type names (bug 1027).  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (double d)
{
  printf ("%s", &d); /* { dg-warning "char \\*" "correct arg type" } */
  scanf ("%zu", &d); /* { dg-warning "size_t \\*" "correct arg type" } */
}
