/* PR tree-optimization/78558 */

#include "tree-vect.h"

struct S
{
  char p[48];
  unsigned long long q, r, s;
} s[50];

struct D
{
  unsigned long long q, r;
} d[50];

void
foo (void)
{
  unsigned long i;
  for (i = 0; i < 50; ++i)
    {
      d[i].q = s[i].q;
      d[i].r = s[i].r;
    }
}

int
main ()
{
  check_vect ();
  unsigned long i;
  for (i = 0; i < 50; ++i)
    {
      s[i].q = i;
      s[i].r = 50 * i;
    }
  asm volatile ("" : : "g" (s), "g" (d) : "memory");
  foo ();
  asm volatile ("" : : "g" (s), "g" (d) : "memory");
  for (i = 0; i < 50; ++i)
    if (d[i].q != i || d[i].r != 50 * i)
      abort ();
  return 0;
}
