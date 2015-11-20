// { dg-do compile }

class C { int a; char b; void foo (); };

void
C::foo ()
{
  #pragma omp parallel shared (a, a)	// { dg-error "appears more than once in data clauses" }
  ;
  #pragma omp parallel shared (a) private (b) shared(C::a)	// { dg-error "appears more than once in data clauses" }
  ;
  #pragma omp task private (a) private (b)
  ;
  #pragma omp task firstprivate (a) shared (C::a)	// { dg-error "appears more than once in data clauses" }
  ;
  #pragma omp parallel for lastprivate (b) firstprivate (a) lastprivate (b)	// { dg-error "appears more than once in data clauses" }
  for (int i = 0; i < 64; i++)
    ;
  #pragma omp parallel for lastprivate (b) firstprivate (b)
  for (int i = 0; i < 64; i++)
    ;
}
