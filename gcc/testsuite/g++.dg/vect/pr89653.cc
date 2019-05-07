// { dg-do compile }
// { dg-require-effective-target vect_double }

#include <algorithm>

void loop1(double * const __restrict__ vec, double x, int end)
{
  for (int i = 0; i < end; ++i)
    vec[i] = std::min(vec[i], vec[i]/x);
}

// { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } }
