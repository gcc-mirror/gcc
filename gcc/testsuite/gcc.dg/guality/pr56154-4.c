/* PR debug/56154 */
/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-additional-sources "pr56154-aux.c" } */

#include "../nop.h"

extern void abort (void);

volatile int z;

__attribute__((noinline, noclone)) int
foo (int x)
{
  z = 6;
  x++;
  x++;
  x++;
  x++;
  x++;
  x++;
  x++;
  x++;
  asm (NOP : : : "memory");
  asm (NOP : : : "memory");	/* { dg-final { gdb-test pr56154-4.c:25 "x" "28" } } */
  return x;
}

void
test_main (void)
{
  if (foo (20) != 28)
    abort ();
}
