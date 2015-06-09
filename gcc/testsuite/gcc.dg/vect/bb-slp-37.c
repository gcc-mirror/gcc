/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

extern void abort (void);

int a[16];
int b[4];

void __attribute__((noinline))
foo (void)
{
  b[0] = a[0];
  b[1] = a[4];
  b[2] = a[8];
  b[3] = a[12];
}

int main()
{
  int i;
  check_vect ();
  for (i = 0; i < 16; ++i)
    {
      a[i] = i;
      __asm__ volatile ("");
    }
  foo ();
  if (b[0] != 0 || b[1] != 4 || b[2] != 8 || b[3] != 12)
    abort ();
  return 0;
}
