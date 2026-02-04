/* { dg-do run } */

#include <stdio.h>

int a;

void __attribute__((noipa))
foo()
{
  unsigned long b = (unsigned long) &a - 134518548;
  volatile unsigned long c = b;
  if (c == 0) {
    if (b != 0) __builtin_abort ();
  }
  a = c;
}

int main()
{
  foo ();
  return 0;
}
