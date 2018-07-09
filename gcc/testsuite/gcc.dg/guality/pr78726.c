/* PR tree-optimization/78726 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

unsigned char b = 36, c = 173;
unsigned int d;

__attribute__((noinline, noclone)) void
foo (void)
{
  unsigned a = ~b;
  unsigned d1 = a * c;		/* { dg-final { gdb-test 21 "d1" "~36U * 173" } } */
  unsigned d2 = d1 * c;		/* { dg-final { gdb-test 21 "d2" "~36U * 173 * 173" } } */
  unsigned d3 = 1023094746 * a;	/* { dg-final { gdb-test 21 "d3" "~36U * 1023094746" } } */
  d = d2 + d3;
  unsigned d4 = d1 * 2;     	/* { dg-final { gdb-test .+3 "d4" "~36U * 173 * 2" } } */
  unsigned d5 = d2 * 2;		/* { dg-final { gdb-test .+2 "d5" "~36U * 173 * 173 * 2" } } */
  unsigned d6 = d3 * 2;		/* { dg-final { gdb-test .+1 "d6" "~36U * 1023094746 * 2" } } */
  asm (NOP : : : "memory");
}

int
main ()
{
  asm volatile ("" : : "g" (&b), "g" (&c) : "memory");
  foo ();
  return 0;
}
