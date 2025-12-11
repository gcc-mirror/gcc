// PR c++/94477
// { dg-do compile }
// { dg-additional-options "-Wno-deprecated-openmp" }
void foo ();

template <int>
void
bar ()
{
  #pragma omp parallel master
  foo ();
}

void
baz ()
{
  bar<0> ();
}
