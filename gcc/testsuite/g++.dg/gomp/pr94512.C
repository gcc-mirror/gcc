// PR c++/94512

void
foo ();

template <int>
void
bar ()
{
#pragma omp parallel master taskloop
  foo ();	// { dg-error "for statement expected before" }
}

void
baz ()
{
  bar<0> ();
}
