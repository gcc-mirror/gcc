/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */
#include <stdlib.h>

#define N 10

struct S
{
  int a, b;
  int *ptr;
  int c, d;
};

int
main (void)
{
  struct S a;
  a.ptr = (int *) malloc (sizeof (int) * N);

  for (int i = 0; i < N; i++)
    a.ptr[i] = 0;

  #pragma omp target enter data map(to: a.ptr, a.ptr[:N])

  #pragma omp target
  for (int i = 0; i < N; i++)
    a.ptr[i] += 1;

  #pragma omp target update from(a.ptr[:N])

  for (int i = 0; i < N; i++)
    if (a.ptr[i] != 1)
      abort ();

  #pragma omp target map(a.ptr[:N])
  for (int i = 0; i < N; i++)
    a.ptr[i] += 1;

  #pragma omp target update from(a.ptr[:N])

  for (int i = 0; i < N; i++)
    if (a.ptr[i] != 2)
      abort ();

  #pragma omp target exit data map(from:a.ptr, a.ptr[:N])

  return 0;
}

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* map\(tofrom:a \[len: [0-9]+\]\[implicit\]\)} "gimple" } } */

/* { dg-final { scan-tree-dump {#pragma omp target num_teams.* map\(tofrom:a \[len: [0-9]+\]\[implicit\]\) map\(tofrom:\*_[0-9]+ \[len: [0-9]+\]\) map\(attach:a\.ptr \[bias: 0\]\)} "gimple" } } */
