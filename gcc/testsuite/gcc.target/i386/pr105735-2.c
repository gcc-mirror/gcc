/* { dg-do run } */
/* { dg-options "-O1" } */

#include "pr105735-1.c"

int main()
{
  unsigned int tmp = 0x1101;
  unsigned int bit2 = 0x111101;
  if (foo (tmp, bit2) != 0x1101)
    __builtin_abort (); 
  if (foo1 (tmp, bit2) != 0x1101)
    __builtin_abort ();
  if (foo2 (tmp, bit2) != 0x111101)
    __builtin_abort ();
  if (foo3 (tmp, bit2) != 0x111101)
    __builtin_abort ();
  if (foo4 (tmp, bit2) != 0x1101)
    __builtin_abort ();
  if (foo5 (tmp, bit2) != 0x110000)
    __builtin_abort ();
  if (f (tmp, 64, bit2) != 0x1101)
    __builtin_abort ();
  if (f1 (tmp, 64, bit2) != 0x111101)
    __builtin_abort ();
  if (f2 (tmp, 64, bit2) != 0x1101)
    __builtin_abort ();
}
