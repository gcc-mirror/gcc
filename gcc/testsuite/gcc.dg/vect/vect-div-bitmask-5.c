/* { dg-require-effective-target vect_int } */

#include <stdint.h>
#include <stdio.h>
#include "tree-vect.h"

#define N 50
#define TYPE uint8_t 

#ifndef DEBUG
#define DEBUG 0
#endif

#define BASE ((TYPE) -1 < 0 ? -126 : 4)


__attribute__((noipa, noinline, optimize("O1")))
void fun1(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] + level) / 0xff;
}

__attribute__((noipa, noinline, optimize("O3")))
void fun2(TYPE* restrict pixel, TYPE level, int n)
{
  for (int i = 0; i < n; i+=1)
    pixel[i] = (pixel[i] + level) / 0xff;
}

int main ()
{
  TYPE a[N];
  TYPE b[N];

  for (int i = 0; i < N; ++i)
    {
      a[i] = BASE + i * 13;
      b[i] = BASE + i * 13;
      if (DEBUG)
        printf ("%d: 0x%x\n", i, a[i]);
    }

  fun1 (a, N / 2, N);
  fun2 (b, N / 2, N);

  for (int i = 0; i < N; ++i)
    {
      if (DEBUG)
        printf ("%d = 0x%x == 0x%x\n", i, a[i], b[i]);

      if (a[i] != b[i])
        __builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump "divmod pattern recognized" "vect" { target aarch64*-*-* } } } */
