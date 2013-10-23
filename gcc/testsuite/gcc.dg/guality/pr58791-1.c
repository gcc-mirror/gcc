/* PR tree-optimization/58791 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

__attribute__((noinline, noclone)) int
foo (int x, int y)
{
  _Bool a = x != 0;
  _Bool b = y != 2;
  _Bool c = a & b;
  _Bool d = !c;
  int ret;
  if (c)
    {
      if (y < 3 || y > 4)
	ret = 1;
      else
	ret = 0;
    }
  else
    ret = 0;
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr58791-1.c:25 "c & 1" "1" } } */
  asm volatile (NOP : : : "memory");	/* { dg-final { gdb-test pr58791-1.c:25 "d & 1" "0" } } */
  return ret;
}

int
main ()
{
  foo (1, 3);
  return 0;
}
