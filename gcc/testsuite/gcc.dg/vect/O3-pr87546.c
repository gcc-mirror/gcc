#include "tree-vect.h"

int a;
long b, f;
signed char c, g;
short int d = 219;
int e[64];

__attribute__((noipa)) void
foo (void)
{
  asm volatile ("" : : "g" (&a), "g" (&d) : "memory");
  for (c = 0; c < 64; c++)
    {
      g = d < 0 ? d : d >> a;
      f = g + b;
      e[c] = f;
    }
  if (e[1] != (signed char) d)
    __builtin_abort ();
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}
