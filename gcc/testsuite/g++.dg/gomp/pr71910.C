// PR target/71910
// { dg-do compile }
// { dg-additional-options "-O2" }

#include <vector>

int
main ()
{
  std::vector<double> vec(10);
#pragma omp parallel
  __builtin_exit (0);
}
