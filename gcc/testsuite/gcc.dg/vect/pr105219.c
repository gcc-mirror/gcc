/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mtune=intel" { target x86_64-*-* i?86-*-* } } */
/* { dg-additional-options "-mtune=thunderx" { target aarch64*-*-* } } */

#include "tree-vect.h"

int data[128];

void __attribute((noipa))
foo (int *data, int n)
{
  for (int i = 0; i < n; ++i)
    data[i] = i;
}

int main()
{
  check_vect ();
  for (int start = 0; start < 16; ++start)
    for (int n = 1; n < 3*16; ++n)
      {
        __builtin_memset (data, 0, sizeof (data));
        foo (&data[start], n);
#pragma GCC novector
        for (int j = 0; j < n; ++j)
          if (data[start + j] != j)
            __builtin_abort ();
      }
  return 0;
}
