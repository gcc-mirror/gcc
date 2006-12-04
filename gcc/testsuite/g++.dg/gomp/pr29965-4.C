// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

extern void baz () __attribute__ ((noreturn));

void
foo1 ()
{
  int i;
#pragma omp for schedule (dynamic)
  for (i = 0; i < 2834; i++)
    for (;;)
      ;
}

void
bar1 ()
{
  int i;
#pragma omp for schedule (dynamic)
  for (i = 0; i < 2834; i++)
    baz ();
}

void
foo2 ()
{
  int i;
#pragma omp parallel for schedule (dynamic)
  for (i = 0; i < 2834; i++)
    for (;;)
      ;
}

void
bar2 ()
{
  int i;
#pragma omp parallel for schedule (dynamic)
  for (i = 0; i < 2834; i++)
    baz ();
}
