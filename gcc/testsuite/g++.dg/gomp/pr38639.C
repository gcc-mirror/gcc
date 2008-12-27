// PR c++/38639
// { dg-do compile }
// { dg-options "-fopenmp -std=c++0x" }

template<int> void
foo ()
{
#pragma omp parallel for
  for (auto i = i = 0; i<4; ++i)	// { dg-error "incomplete|unable|invalid" }
    ;
}

void
bar ()
{
  foo<0> ();				// { dg-message "instantiated from here" }
}
