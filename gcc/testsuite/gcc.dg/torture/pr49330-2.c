/* { dg-do run } */

#include <stdio.h>

int a;

void __attribute__((noipa))
foo()
{
  __UINTPTR_TYPE__ b = (__UINTPTR_TYPE__) &a - 134518548;
  volatile __UINTPTR_TYPE__ c = b;
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
