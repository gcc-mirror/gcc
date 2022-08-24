/* { dg-do run } */
/* { dg-options "-O1" } */

#include "pr103462-5.c"

int main()
{
  unsigned short tmp = 0x1111U;

  if (foo (tmp) != 0x110)
    __builtin_abort ();

  if (foo1 (tmp) != 0x110)
    __builtin_abort ();

  if (foo2 (tmp) != 0x0)
    __builtin_abort ();

  if (foo3 (tmp) != 0x0)
    __builtin_abort ();

  if (foo4 (tmp) != 0xffff)
    __builtin_abort ();

  if (foo5 (tmp) != 0xffff)
    __builtin_abort ();

  if (foo6 (tmp) != 0x9359)
    __builtin_abort ();

  if (foo7 (tmp) != 0x9359)
    __builtin_abort ();

  if (foo8 (tmp) != 0x8358)
    __builtin_abort ();

  if (foo9 (tmp) != 0x8358)
    __builtin_abort ();

  if (foo10 (tmp) != 0x8358)
    __builtin_abort ();

  if (foo11 (tmp) != 0x8358)
    __builtin_abort ();
}

