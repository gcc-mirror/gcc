/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow" } */

#include <stdio.h>

int
main (void)
{
  fputs ("UBSAN TEST START\n", stderr);

  long long int a = 14;
  long int b = 9;
  asm volatile ("" : "+r" (a), "+r" (b));
  if ((a - b) != 5)
    __builtin_abort ();

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
