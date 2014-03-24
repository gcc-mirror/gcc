/* { dg-do run } */
/* { dg-options "-fsanitize=shift -w" } */

#include <stdio.h>

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  unsigned int a = 1;
  a <<= 31;
  a <<= 1;

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
