// { dg-do compile }
// { dg-additional-options "-std=c++11" }

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

#pragma omp tile sizes (U, 10, V)
  for (T i : v)
  for (T j : v)
  for (T k : v)
    dummy (i);
}

void test () { test1_template <long, 5, 3> (); };
