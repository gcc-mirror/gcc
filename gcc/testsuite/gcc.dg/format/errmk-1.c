/* Test for format checking not giving tree checking errors.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

void
foo (int t)
{
  printf ("%*d", u, t); /* { dg-error "undeclared|function" "u undeclared error" } */
}
