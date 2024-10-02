/* { dg-do run } */
/* { dg-options "-O1" } */

#include <stdint.h>
#include <limits.h>
#include <stdio.h>

#ifndef N
#define N 65
#endif

#ifndef TYPE
#define TYPE int32_t
#endif

#ifndef DEBUG
#define DEBUG 1
#endif

#define BASE ((TYPE) -1 < 0 ? -126 : 4)

__attribute__ ((noinline, noipa))
void fun1(TYPE *x, int n)
{
    for (int i = 0; i < n; i++)
      x[i] = (-x[i]) >> 31;
}

__attribute__ ((noinline, noipa, optimize("O0")))
void fun2(TYPE *x, int n)
{
    for (int i = 0; i < n; i++)
      x[i] = (-x[i]) >> 31;
}

int main ()
{
  TYPE a[N];
  TYPE b[N];

  /* This will invoke UB due to -INT32_MIN.  The test is supposed to pass
     because GCC is supposed to handle this UB case in a predictable way.  */
  a[0] = INT32_MIN;
  b[0] = INT32_MIN;

  for (int i = 1; i < N; ++i)
    {
      a[i] = BASE + i * 13;
      b[i] = BASE + i * 13;
      if (DEBUG)
        printf ("%d: 0x%x\n", i, a[i]);
    }

  fun1 (a, N);
  fun2 (b, N);

  if (DEBUG)
    printf ("%d = 0x%x == 0x%x\n", 0, a[0], b[0]);

  if (a[0] != 0x0 || b[0] != -1)
        __builtin_abort ();


  for (int i = 1; i < N; ++i)
    {
      if (DEBUG)
        printf ("%d = 0x%x == 0x%x\n", i, a[i], b[i]);

      if (a[i] != b[i])
        __builtin_abort ();
    }
  return 0;
}

