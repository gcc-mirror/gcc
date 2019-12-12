/* { dg-do compile } */
/* { dg-options "-Wsign-compare -std=c99" } */

#include <stdbool.h>

int foo()
{
  unsigned char b = '1';

  bool x = ~b; /* { dg-warning "promoted bitwise complement of an unsigned value is always nonzero" } */

  return 0;
}
