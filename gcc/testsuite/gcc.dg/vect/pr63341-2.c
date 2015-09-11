/* PR tree-optimization/63341 */

#include "tree-vect.h"

typedef union U { unsigned short s; unsigned char c; } __attribute__((packed)) U;
struct S { char e __attribute__((aligned (64))); U s[32]; };
struct S t = {0, {{0x5010}, {0x5111}, {0x5212}, {0x5313}, {0x5414}, {0x5515}, {0x5616}, {0x5717},
		  {0x5818}, {0x5919}, {0x5a1a}, {0x5b1b}, {0x5c1c}, {0x5d1d}, {0x5e1e}, {0x5f1f},
		  {0x6020}, {0x6121}, {0x6222}, {0x6323}, {0x6424}, {0x6525}, {0x6626}, {0x6727},
		  {0x6828}, {0x6929}, {0x6a2a}, {0x6b2b}, {0x6c2c}, {0x6d2d}, {0x6e2e}, {0x6f2f}}};
unsigned short d[32] = { 1 };

__attribute__((noinline, noclone)) void
foo ()
{
  int i;
  for (i = 0; i < 32; i++)
    d[i] = t.s[i].s + 4;
  for (i = 0; i < 32; i++)
    if (d[i] != t.s[i].s + 4)
      abort ();
    else
      asm volatile ("" : : : "memory");
}

int
main ()
{
  check_vect ();
  foo ();
  return 0;
}

