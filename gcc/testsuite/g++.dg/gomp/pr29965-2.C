// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

extern void baz () __attribute__ ((noreturn));

void
foo1 ()
{
#pragma omp sections
  {
    for (;;)
      ;
  }
}

void
bar1 ()
{
#pragma omp sections
  {
#pragma omp section
    baz ();
#pragma omp section
    baz ();
  }
}

void
foo2 ()
{
#pragma omp sections
  {
    ;
#pragma omp section
    for (;;)
      ;    
  }
}

void
bar2 ()
{
#pragma omp sections
  {
#pragma omp section
    baz ();
#pragma omp section
    ;
  }
}

void
foo3 ()
{
#pragma omp parallel sections
  {
    for (;;)
      ;
  }
}

void
bar3 ()
{
#pragma omp parallel sections
  {
#pragma omp section
    baz ();
#pragma omp section
    baz ();
  }
}

void
foo4 ()
{
#pragma omp parallel sections
  {
    ;
#pragma omp section
    for (;;)
      ;    
  }
}

void
bar4 ()
{
#pragma omp parallel sections
  {
#pragma omp section
    baz ();
#pragma omp section
    ;
  }
}
