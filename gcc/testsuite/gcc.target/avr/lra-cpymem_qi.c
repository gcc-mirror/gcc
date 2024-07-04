/* { dg-do compile } */
/* { dg-options "-Os" } */

#include <stdbool.h>

struct A
{
  unsigned int a;
  unsigned char c1, c2;
  bool b1 : 1;
};

void
foo (const struct A *x, int y)
{
  int s = 0, i;
  for (i = 0; i < y; ++i)
    {
      const struct A a = x[i];
      s += a.b1 ? 1 : 0;
    }
  if (s != 0)
    __builtin_abort ();
}

/* { dg-final { scan-assembler-not "movw\[^\n\r]*r26,r30" } } */
