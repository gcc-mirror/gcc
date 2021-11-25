#include <stdio.h>

#ifndef N
#define N 65
#endif

#ifndef TYPE
#define TYPE uint32_t
#endif

#ifndef DEBUG
#define DEBUG 0
#endif

#define BASE ((TYPE) -1 < 0 ? -126 : 4)

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

  fun1 (a, N);
  fun2 (b, N);

  for (int i = 0; i < N; ++i)
    {
      if (DEBUG)
        printf ("%d = 0x%x == 0x%x\n", i, a[i], b[i]);

      if (a[i] != b[i])
        __builtin_abort ();
    }
  return 0;
}

