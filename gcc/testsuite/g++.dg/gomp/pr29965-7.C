// PR middle-end/29965
// Test that OpenMP construct bodies which never return don't cause ICEs.
// This is invalid code, but we don't emit diagnostics for it, nevertheless
// we test that we don't ICE on it.
// { dg-do compile }
// { dg-options "-O2 -fopenmp" }

void
foo ()
{
#pragma omp parallel
  throw 0;
}

static inline void
bar ()
{
#pragma omp parallel
  throw 0;
}

void
bar1 ()
{
  bar ();
}

void
bar2 ()
{
  bar ();
}
