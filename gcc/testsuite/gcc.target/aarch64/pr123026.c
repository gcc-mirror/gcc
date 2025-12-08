/* { dg-do run } */
/* { dg-additional-options "-O3 -march=armv8-a -std=c99" } */

#include <stdbool.h>

int g;

__attribute__ ((noipa)) void
foo(bool a) {
  for (int i = 0; i < 4; i++)
    if (!i || a)
      g += 1;
}

int main()
{
  foo(0);
  if (g != 1)
    __builtin_abort();
  return 0;
}
