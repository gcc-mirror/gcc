// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

extern void dummy (int);

template<class T, int U1, int U2, int U3> void
test_template ()
{
  std::vector<int> v;

  for (unsigned i = 0; i < 1000; i++)
    v.push_back (i);

#pragma omp for
  for (int i : v)
    dummy (i);

#pragma omp unroll partial(U1)
  for (T i : v)
    dummy (i);

#pragma omp unroll partial(U2) // { dg-error "'partial' argument needs positive constant integer expression" }
  for (T i : v)
    dummy (i);

#pragma omp unroll partial(U3) // { dg-error "'partial' argument needs positive constant integer expression" }
  for (T i : v)
    dummy (i);

#pragma omp for
#pragma omp unroll partial(U1)
  for (T i : v)
    dummy (i);

#pragma omp for
#pragma omp unroll partial(U2) // { dg-error "'partial' argument needs positive constant integer expression" }
  for (T i : v)
    dummy (i);

#pragma omp for
#pragma omp unroll partial(U3) // { dg-error "'partial' argument needs positive constant integer expression" }
  for (T i : v)
    dummy (i);
}

void
test ()
{
  test_template <long, 5,-2, 0> ();
}
