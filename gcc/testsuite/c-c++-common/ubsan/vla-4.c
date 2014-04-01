/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */

#include <stdio.h>

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  int x = 1;
  /* Check that the size of an array is evaluated only once.  */
  int a[++x];
  if (x != 2)
    __builtin_abort ();

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
