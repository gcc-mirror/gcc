// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// This is invalid code, but we don't emit diagnostics for it, nevertheless
// we test that we don't ICE on it.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

void
foo1 ()
{
  int i;
#pragma omp for schedule (static)
  for (i = 0; i < 2834; i++)
    throw 0;
}

void
foo2 ()
{
  int i;
#pragma omp parallel for schedule (static)
  for (i = 0; i < 2834; i++)
    throw 0;
}
