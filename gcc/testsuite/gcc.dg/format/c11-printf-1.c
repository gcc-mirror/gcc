/* Test for printf formats: rejection of C2X (and C2X-recommended) formats in
   pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wformat" } */

#include "format.h"

void
foo (int i)
{
  printf ("%b", i); /* { dg-warning "C" } */
  printf ("%B", i); /* { dg-warning "C" } */
}
