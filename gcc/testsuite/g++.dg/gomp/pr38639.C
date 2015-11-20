// PR c++/38639
// { dg-do compile { target c++11 } }
// { dg-options "-fopenmp" }

template<int> void
foo ()
{
#pragma omp parallel for
  for (auto i = i = 0; i<4; ++i)	// { dg-error "initializer expression refers to iteration variable" }
    ;
}

void
bar ()
{
  foo<0> ();
}
