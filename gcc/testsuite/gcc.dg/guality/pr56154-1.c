/* PR debug/56154 */
/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-additional-sources "pr56154-aux.c" } */

#include "../nop.h"

union U { int a, b; };
volatile int z;

__attribute__((noinline, noclone)) int
foo (int fd, union U x)
{
  int result = x.a != 0;
  if (fd != 0)
    result = x.a == 0;
  asm (NOP : : : "memory");	  /* { dg-final { gdb-test pr56154-1.c:17 "x.a" "4" } } */
  z = x.a;
  x.a = 6;
  asm (NOP : : : "memory");	  /* { dg-final { gdb-test pr56154-1.c:20 "x.a" "6" } } */
  return result;
}

void
test_main (void)
{
  union U u = { .a = 4 };
  foo (0, u);
}
