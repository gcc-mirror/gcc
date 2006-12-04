/* PR middle-end/29965 */
/* Test that OpenMP construct bodies which never return don't cause ICEs.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */

extern void baz (void) __attribute__ ((noreturn));

void
foo1 (void)
{
#pragma omp single
  for (;;);
}

void
bar1 (void)
{
#pragma omp single
  baz ();
}

void
foo2 (void)
{
#pragma omp master
  for (;;);
}

void
bar2 (void)
{
#pragma omp master
  baz ();
}

void
foo3 (void)
{
#pragma omp ordered
  for (;;);
}

void
bar3 (void)
{
#pragma omp ordered
  baz ();
}

void
foo4 (void)
{
#pragma omp critical
  for (;;);
}

void
bar4 (void)
{
#pragma omp critical
  baz ();
}
