// PR c++/31748

struct A;

void
foo ()
{
#pragma omp parallel private(A)	// { dg-error "struct A.*is not a variable" }
  ;
}
