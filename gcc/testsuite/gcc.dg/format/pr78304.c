/* { dg-do compile { target inttypes_types } } */
/* { dg-options "-O2 -Wall -Wextra" } */

#include <inttypes.h>
#include <stdio.h>

void test (size_t size)
{
  printf ("size: %" PRIu32 "\n", size); /* { dg-warning "expects argument of type" } */
}
