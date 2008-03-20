/* Test for strange warning in format checking.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (void *p)
{
  printf ("%d", p); /* { dg-bogus "va_list" "wrong type in format warning" } */
  /* { dg-warning "format" "format error" { target *-*-* } 12 } */
}
