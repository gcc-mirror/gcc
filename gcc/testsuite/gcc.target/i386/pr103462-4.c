/* { dg-do run } */
/* { dg-options "-O1" } */

#include "pr103462-3.c"

int main()
{
  unsigned int tmp = 0x11111111U;

  if (foo (tmp) != 0x10110110U)
    __builtin_abort ();

  if (foo1 (tmp) != 0x1101101U)
    __builtin_abort ();

  if (foo2 (tmp) != 0x0U)
    __builtin_abort ();

  if (foo3 (tmp) != 0x0U)
    __builtin_abort ();

  if (foo4 (tmp) != 0xffffffffU)
    __builtin_abort ();

  if (foo5 (tmp) != 0xffffffffU)
    __builtin_abort ();

  if (foo6 (tmp) != 0x59359359U)
    __builtin_abort ();

  if (foo7 (tmp) != 0x93593593U)
    __builtin_abort ();

  if (foo8 (tmp) != 0xa7ca7ca7U)
    __builtin_abort ();

  if (foo9 (tmp) != 0x7ca7ca7cU)
    __builtin_abort ();

  if (foo10 (tmp) != 0x58358358U)
    __builtin_abort ();

  if (foo11 (tmp) != 0x83583583U)
    __builtin_abort ();
}

