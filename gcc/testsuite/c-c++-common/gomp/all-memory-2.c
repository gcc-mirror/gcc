/* { dg-options "-fno-openmp" } */

int omp_all_memory;			/* { dg-bogus "expected" } */

void
foo (void)
{
  int p = (&omp_all_memory)[0];		/* { dg-bogus "'omp_all_memory' may only be used in OpenMP 'depend' clause" } */
}

void
bar (void)
{
  int *omp_all_memory;			/* { dg-bogus "expected" } */
}

void
baz (void)
{
  struct omp_all_memory { int a; };	/* { dg-bogus "expected" } */
}

void
qux (void)
{
  union omp_all_memory { int a; };	/* { dg-bogus "expected" } */
}

void
corge (void)
{
  enum omp_all_memory { OAM };		/* { dg-bogus "expected" } */
}

void
garply (void)
{
  enum E { omp_all_memory };		/* { dg-bogus "expected" } */
}

void
boo (void)
{
  int x, y;
  #pragma omp task private (omp_all_memory)
  ;
  #pragma omp task depend(inout: *&omp_all_memory)
  ;
  #pragma omp task depend(inout: omp_all_memory[0])
  ;
  #pragma omp task depend(in: omp_all_memory)
  ;
  #pragma omp task depend(mutexinoutset: omp_all_memory)
  ;
  #pragma omp task depend(inoutset: omp_all_memory)
  ;
}
