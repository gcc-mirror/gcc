/* Test for format diagnostics.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (double d)
{
  /* This should get a message referring to `hh', not to `H'.  */
  printf ("%hhf", d); /* { dg-warning "hh" "%hhf warning" } */
  /* This should get a message referring to `ll', not to `q'.  */
  printf ("%llf", d); /* { dg-warning "ll" "%llf warning" } */
  /* This should get a message referring to 'size_t', not to
     'unsigned int' or similar.  */
  printf ("%zu", d); /* { dg-warning "size_t" "size_t format warning" } */
}
