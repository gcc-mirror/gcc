#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 128
#define NINTS (VECTOR_BITS / 32)
#define EXTRA (NINTS * 2)
#else
#define NINTS 4
#define EXTRA 10
#endif

#define N 128

#define RES_A (N * N / 4)
#define RES_B (N * (N + 1) / 2 + (NINTS + 3) * (N + 1))
#define RES_C (N * (N + 1) / 2 + (N + 1))
#define RES (RES_A + RES_B + RES_C)

int ib[N + EXTRA];
int ia[N + EXTRA];
int ic[N + EXTRA];

__attribute__ ((noinline))
int main1 ()
{
  int i, suma = 0, sumb = 0, sumc = 0;

  /* ib and ic have same misalignment, we peel to align them.  */
  for (i = 0; i <= N; i++)
    {
      suma += ia[i];
      sumb += ib[i + NINTS + 1];
      sumc += ic[i + 1];
    }

  /* check results:  */
  if (suma + sumb + sumc != RES)
    abort ();

  return 0;
}

int main (void)
{
  int i;

  check_vect ();

  for (i = 0; i < N + EXTRA; i++)
    {
      asm volatile ("" : "+r" (i));
      ib[i] = i;
      ic[i] = i+2;
      ia[i] = i/2;
    }

  return main1 ();
}
