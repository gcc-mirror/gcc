/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */
/* { dg-additional-options "-std=gnu11" { target c } } */
/* { dg-additional-options "-std=c++11" { target c++ } } */

#include <stdio.h>

volatile int w, z;

__attribute__ ((noinline, noclone)) int
foo (int x, int y)
{
  z++;
  return x << y;
}

int
main ()
{
  fputs ("1st\n", stderr);
  w = foo (0, -__INT_MAX__);
  return 0;
}

/* { dg-output "1st(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*shift exponent -\[^\n\r]* is negative\[^\n\r]*(\n|\r\n|\r)" } */
