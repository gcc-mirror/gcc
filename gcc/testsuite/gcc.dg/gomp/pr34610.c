/* PR gcov-profile/34610 */
/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-arcs -fopenmp" } */

extern void bar (int);
extern void baz (int) __attribute__((noreturn));

void
foo (int k)
{
  int i;
#pragma omp for schedule(dynamic)
  for (i = 0; i < 10; ++i)
    bar (i);
#pragma omp parallel for schedule(static)
  for (i = 0; i < 10; ++i)
    bar (i);
#pragma omp parallel for schedule(static, 4)
  for (i = 0; i < 10; ++i)
    bar (i);
  if (k)
  #pragma omp for schedule(dynamic)
    for (i = 0; i < 10; ++i)
      baz (i);
#pragma omp parallel
  for (i = 0; i < 10; ++i)
    bar (i);
}

/* { dg-final { cleanup-coverage-files } } */
