// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// This is invalid code, but we don't emit diagnostics for it, nevertheless
// we test that we don't ICE on it.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

void
foo1 ()
{
#pragma omp single
  throw 0;
}

void
foo2 ()
{
#pragma omp master
  throw 0;
}

void
foo3 ()
{
#pragma omp ordered
  throw 0;
}

void
foo4 ()
{
#pragma omp critical
  throw 0;
}
