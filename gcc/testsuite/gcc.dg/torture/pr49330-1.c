/* { dg-do run } */

#include <stdint.h>

int x, y;
int __attribute__((noipa)) foo()
{
  uintptr_t px = (uintptr_t) &x;
  uintptr_t py = (uintptr_t) &y;
  volatile uintptr_t d = px - py;
  uintptr_t p = py + d;
  x = 1;
  *(int *) p = 2;
  return x;
}

int main()
{
  if (foo () != 2)
    __builtin_abort ();
  return 0;
}
