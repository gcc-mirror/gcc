/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch" } */

#include <stdio.h>

int hp1(void)
{
  int nested1(void) /* { dg-warning "hotpatching is not compatible with nested functions" } */
  { return 1; }

  __attribute__ ((hotpatch))
  int nested2(void) /* { dg-warning "hotpatching is not compatible with nested functions" } */
  { return 1; }

  return nested1() - nested2();
}

int main (void)
{
  return hp1();
}
