/* Test for strange warning in format checking.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

#include "format.h"

void
foo (void *p)
{
  printf ("%d", p); /* { dg-bogus "va_list" "wrong type in format warning" } */
  /* { dg-warning "format" "format error" { target *-*-* } .-1 } */
}
