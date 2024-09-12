// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

extern void dummy (int);

constexpr unsigned
fib (unsigned n)
{
  return n <= 2 ? 1 : fib (n-1) + fib (n-2);
}

void
test1 ()
{
  std::vector<int> v;

  for (unsigned i = 0; i < 1000; i++)
    v.push_back (i);

#pragma omp unroll partial(fib(10))
  for (int i : v)
    dummy (i);
}


// Loop should be unrolled fib(10) = 55 times
// { dg-final { scan-tree-dump {#pragma omp unroll partial\(55\)} "original" } }
// { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } }
