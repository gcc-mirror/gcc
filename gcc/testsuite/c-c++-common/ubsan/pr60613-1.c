/* PR sanitizer/60613 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

#include <stdio.h>

long long y;

__attribute__((noinline, noclone)) long long
foo (long long x)
{
  asm ("");
  if (x >= 0 || x < -2040)
    return 23;
  x += 2040;
  return x - y;
}

__attribute__((noinline, noclone)) long long
bar (long long x)
{
  asm ("");
  return 8LL - x;
}

int
main ()
{
  fputs ("UBSAN TEST START\n", stderr);

  y = 1;
  if (foo (8 - 2040) != 8 - 1)
    __builtin_abort ();
  if (bar (1) != 8 - 1)
    __builtin_abort ();

  fputs ("UBSAN TEST END\n", stderr);
  return 0;
}

/* { dg-output "UBSAN TEST START(\n|\r\n|\r)UBSAN TEST END" } */
