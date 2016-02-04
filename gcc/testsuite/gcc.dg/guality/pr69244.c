/* PR debug/69244 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

union U { float f; int i; };
float a, b;

__attribute__((noinline, noclone)) void
foo (void)
{
  asm volatile ("" : : "g" (&a), "g" (&b) : "memory");
}

int
main ()
{
  float e = a;
  foo ();
  float d = e;
  union U p;
  p.f = d += 2;
  int c = p.i - 4;
  asm (NOP : : : "memory");
  b = c;
  return 0;
}

/* { dg-final { gdb-test 25 "c" "p.i-4" } } */
