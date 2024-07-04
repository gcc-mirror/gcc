/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target apxf } */
/* { dg-options "-mapxf -march=x86-64 -O2" } */
#include "apx-zu-1.c"

int main(void)
{
  if (!__builtin_cpu_supports ("apxf"))
    return 0;

  if (foo0 (0))
    __builtin_abort ();
  if (foo1 (3, 2))
    __builtin_abort ();
  if (foo2 (3, 2))
    __builtin_abort ();
  if (foo3 (2, 3))
    __builtin_abort ();
  if (f1 (2) != 2000)
    __builtin_abort ();
  return 0;
}
