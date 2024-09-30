// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

extern void dummy (int);

template<class T, int U, unsigned V> void
test1_template ()
{
  std::vector<int> v;

  for (unsigned i = 0; i < 10; i++)
    v.push_back (i);

#pragma omp for
  for (int i : v)
    dummy (i);

#pragma omp tile sizes (U, 10, V) // { dg-error "'sizes' argument needs positive integral constant" }
  for (T i : v)
  for (T j : v)
  for (T k : v)
    dummy (i);
}

void
test ()
{
  test1_template <long, 5, 0> ();
}
