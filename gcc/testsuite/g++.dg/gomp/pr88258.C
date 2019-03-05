// PR c++/88258
// { dg-do compile }
// { dg-options "-fopenmp" }

void
foo (bar int p)		// { dg-error "variable or field|was not declared in this scope" }
{
  int i, x;
  #pragma omp atomic write
  x = 6;
}
