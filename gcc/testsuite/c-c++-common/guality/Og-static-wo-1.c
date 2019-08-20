/* { dg-do run } */
/* { dg-options "-g" } */

#include "../../gcc.dg/nop.h"

static int x = 0;

int
main (void)
{
  asm volatile (NOP);		/* { dg-final { gdb-test . "x" "0" } } */
  x = 1;
  asm volatile (NOP);		/* { dg-final { gdb-test . "x" "1" } } */
  return 0;
}
