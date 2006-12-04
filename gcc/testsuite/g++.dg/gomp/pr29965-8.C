// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// This is invalid code, but we don't emit diagnostics for it, nevertheless
// we test that we don't ICE on it.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

void
foo1 ()
{
#pragma omp sections
  {
    throw 0;
  }
}

void
bar1 ()
{
#pragma omp sections
  {
#pragma omp section
    throw 0;
#pragma omp section
    throw 0;
  }
}

void
foo2 ()
{
#pragma omp sections
  {
    ;
#pragma omp section
    throw 0;
  }
}

void
bar2 ()
{
#pragma omp sections
  {
#pragma omp section
    throw 0;
#pragma omp section
    ;
  }
}

void
foo3 ()
{
#pragma omp parallel sections
  {
    throw 0;
  }
}

void
bar3 ()
{
#pragma omp parallel sections
  {
#pragma omp section
    throw 0;
#pragma omp section
    throw 0;
  }
}

void
foo4 ()
{
#pragma omp parallel sections
  {
    ;
#pragma omp section
    throw 0;
  }
}

void
bar4 ()
{
#pragma omp parallel sections
  {
#pragma omp section
    throw 0;
#pragma omp section
    ;
  }
}
