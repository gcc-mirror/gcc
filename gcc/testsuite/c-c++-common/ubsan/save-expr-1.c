/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -Wall -Werror -O" } */

#include <stdio.h>

static int x;
int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  int o = 1;
  int y = x << o;

  fputs ("UBSAN TEST END\n", stderr);
  return y;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
