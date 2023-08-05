/* PR tree-optimization/51074 */

#include "tree-vect.h"

struct S { int a, b; } s[8];

int
main ()
{
  int i;
  check_vect ();
  for (i = 0; i < 8; i++)
    {
      s[i].b = 0;
      s[i].a = i;
    }
  asm volatile ("" : : : "memory");
#pragma GCC novector
  for (i = 0; i < 8; i++)
    if (s[i].b != 0 || s[i].a != i)
      abort ();
  return 0;
}

