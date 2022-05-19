int omp_all_memory;			/* { dg-error "expected" } */

void
foo (void)
{
  int p = (&omp_all_memory)[0];		/* { dg-error "'omp_all_memory' may only be used in OpenMP 'depend' clause" } */
}

void
bar (void)
{
  int *omp_all_memory;			/* { dg-error "expected" } */
}

void
baz (void)
{
  struct omp_all_memory { int a; };	/* { dg-error "expected" } */
}

void
qux (void)
{
  union omp_all_memory { int a; };	/* { dg-error "expected" } */
}

void
corge (void)
{
  enum omp_all_memory { OAM; };		/* { dg-error "expected" } */
}

void
garply (void)
{
  enum E { omp_all_memory }; }		/* { dg-error "expected" } */

void
boo (void)
{
  int x, y;
  #pragma omp task private (omp_all_memory)			/* { dg-error "expected" } */
  ;
  #pragma omp task depend(inout: *&omp_all_memory)		/* { dg-error "'omp_all_memory' may only be used in OpenMP 'depend' clause" } */
  ;
  #pragma omp task depend(inout: omp_all_memory[0])		/* { dg-error "'omp_all_memory' may only be used in OpenMP 'depend' clause" } */
  ;
  #pragma omp task depend(in: omp_all_memory)			/* { dg-error "'omp_all_memory' used with 'depend' kind other than 'out' or 'inout'" } */
  ;
  #pragma omp task depend(mutexinoutset: omp_all_memory)	/* { dg-error "'omp_all_memory' used with 'depend' kind other than 'out' or 'inout'" } */
  ;
  #pragma omp task depend(inoutset: omp_all_memory)		/* { dg-error "'omp_all_memory' used with 'depend' kind other than 'out' or 'inout'" } */
  ;
}
