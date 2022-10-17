/* Test for printf formats: rejection of C2X formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int *uip)
{
  scanf ("%b", uip); /* { dg-warning "C" } */
}
