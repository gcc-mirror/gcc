#include "tree-vect.h"

#define N (VECTOR_BITS / 32)
#define MAX_COUNT 4

#define RUN_COUNT(COUNT)				\
  void __attribute__ ((noipa))				\
  run_##COUNT (int *restrict a, int *restrict b)	\
  {							\
    for (int i = 0; i < N * COUNT; ++i)			\
      {							\
	a[i * 2] = b[i * 2] + COUNT;			\
	a[i * 2 + 1] = COUNT;				\
      }							\
  }

RUN_COUNT (1)
RUN_COUNT (2)
RUN_COUNT (3)
RUN_COUNT (4)

void __attribute__ ((noipa))
check (int *restrict a, int count)
{
#pragma GCC novector
  for (int i = 0; i < count * N; ++i)
    if (a[i * 2] != i * 41 + count || a[i * 2 + 1] != count)
      __builtin_abort ();
  if (a[count * 2 * N] != 999)
    __builtin_abort ();
}

int a[N * MAX_COUNT * 2 + 1], b[N * MAX_COUNT * 2];

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N * MAX_COUNT; ++i)
    {
      b[i * 2] = i * 41;
      asm volatile ("" ::: "memory");
    }

  a[N * 2] = 999;
  run_1 (a, b);
  check (a, 1);

  a[N * 4] = 999;
  run_2 (a, b);
  check (a, 2);

  a[N * 6] = 999;
  run_3 (a, b);
  check (a, 3);

  a[N * 8] = 999;
  run_4 (a, b);
  check (a, 4);

  return 0;
}

/* { dg-final { scan-tree-dump {LOOP VECTORIZED} "vect" { target { { vect_int && vect_perm } && vect_element_align } } } } */
