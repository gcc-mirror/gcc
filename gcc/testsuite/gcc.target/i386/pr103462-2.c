/* { dg-do run } */
/* { dg-options "-O1" } */

#include "pr103462-1.c"

int main()
{
  unsigned long long tmp = 0x1111111111111111ULL;
  if (foo (tmp) != 0x110110110110110ULL)
    __builtin_abort ();

  if (foo1 (tmp) != 0x110110110110110ULL)
    __builtin_abort ();

  if (foo2 (tmp) != 0x0ULL)
    __builtin_abort ();

  if (foo3 (tmp) != 0x0ULL)
    __builtin_abort ();

  if (foo4 (tmp) != 0xffffffffffffffffULL)
    __builtin_abort ();

  if (foo5 (tmp) != 0xffffffffffffffffULL)
    __builtin_abort ();

  if (foo6 (tmp) != 0x9359359359359359ULL)
    __builtin_abort ();

  if (foo7 (tmp) != 0x9359359359359359ULL)
    __builtin_abort ();

  if (foo8 (tmp) != 0x8358358358358358ULL)
    __builtin_abort ();

  if (foo9 (tmp) != 0x8358358358358358ULL)
    __builtin_abort ();

  if (foo10 (tmp) != 0x8358358358358358ULL)
    __builtin_abort ();

  if (foo11 (tmp) != 0x8358358358358358ULL)
    __builtin_abort ();
}

