// PR c++/27359
// { dg-do compile }

void
foo ()
{
#pragma omp parallel for
  for (int i; i < 1; ++i)	// { dg-error "expected|was not declared" }
    ;
}
