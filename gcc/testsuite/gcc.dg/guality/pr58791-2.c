/* PR tree-optimization/58791 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

__attribute__((noinline, noclone)) int
foo (unsigned char c)
{
  int ret;
  _Bool a, b, d, e, f;

  a = c == 34;
  b = c == 32;
  d = a | b;
  f = !d;
  if (d)
    ret = 1;
  else
    {
      e = c <= 31;
      ret = e;
    }

  asm volatile (NOP : : : "memory");     /* { dg-final { gdb-test pr58791-2.c:27 "d & 1" "1" } } */
  asm volatile (NOP : : : "memory");     /* { dg-final { gdb-test pr58791-2.c:27 "f & 1" "0" } } */
  return ret;
}


int
main ()
{
  foo (32);
  return 0;
}
