/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

#include <stdio.h>

int
foo (int x, int y)
{
  const int z = 2;
  if (z & 1)
    return x << y;
  return 0;
}

int
bar (int x, int y)
{
  return x + y;
}

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  foo (3, 2);
  bar (12, 42);

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
