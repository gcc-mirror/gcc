/* PR tree-optimization/58791 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

__attribute__((noinline, noclone)) unsigned
foo (unsigned a, unsigned b, unsigned c, unsigned d, unsigned e)
{
  unsigned f = b + c;		/* { dg-final { gdb-test pr58791-3.c:19 "f" "5" } } */
  unsigned g = a - f;		/* { dg-final { gdb-test pr58791-3.c:19 "g" "24" } } */
  unsigned h = d + e;		/* { dg-final { gdb-test pr58791-3.c:19 "h" "9" } } */
  unsigned i = g - h;		/* { dg-final { gdb-test pr58791-3.c:19 "i" "15" } } */
  unsigned j = f + 1;		/* { dg-final { gdb-test pr58791-3.c:19 "j" "6" } } */
  unsigned k = g + 1;		/* { dg-final { gdb-test pr58791-3.c:19 "k" "25" } } */
  unsigned l = h + 1;		/* { dg-final { gdb-test pr58791-3.c:19 "l" "10" } } */
  unsigned m = i + 1;		/* { dg-final { gdb-test pr58791-3.c:19 "m" "16" } } */
  asm volatile (NOP : : : "memory");
  asm volatile (NOP : : : "memory");
  return i;
}

int
main ()
{
  foo (29, 2, 3, 4, 5);
  return 0;
}
