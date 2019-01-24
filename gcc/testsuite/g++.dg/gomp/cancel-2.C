// PR c++/88976
// { dg-do compile }

template <class T> void
foo (T x)
{
#pragma omp parallel
  {
  #pragma omp cancel parallel if (x)	// { dg-error "no match for" }
  }
}

struct S {};

void
bar ()
{
  S s;
  foo (s);
}
