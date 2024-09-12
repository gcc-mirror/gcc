// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

extern void dummy (int);

void
test1 ()
{
  std::vector<int> v;

  for (unsigned i = 0; i < 1000; i++)
    v.push_back (i);

  #pragma omp for
  for (int i : v)
    dummy (i);

  #pragma omp unroll partial(5)
  for (int i : v)
    dummy (i);
}

void
test2 ()
{
  std::vector<std::vector<int>> v;

  for (unsigned i = 0; i < 10; i++)
    {
      std::vector<int> u;
      for (unsigned j = 0; j < 10; j++)
	u.push_back (j);
      v.push_back (u);
    }

  #pragma omp for
  #pragma omp unroll partial(5)
  for (auto u : v)
    for (int i : u)
      dummy (i);
}
