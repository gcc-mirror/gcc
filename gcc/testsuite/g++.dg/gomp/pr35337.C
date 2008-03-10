// PR c++/35337
// { dg-do compile }
// { dg-options "-fopenmp" }

struct A { };

void
foo ()
{
#pragma omp parallel firstprivate(A)	// { dg-error "struct A\[^\n\]*is not a variable" }
  ;
}

void
bar ()
{
#pragma omp for lastprivate(A)		// { dg-error "struct A\[^\n\]*is not a variable" }
  for (int i = 0; i < 10; i++)
    ;
}
