/* { dg-do run { target c++11 } } */
/* { dg-options "-fsanitize=shift -w" } */

#include <stdio.h>

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  int a = 1;
  a <<= 31;

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
