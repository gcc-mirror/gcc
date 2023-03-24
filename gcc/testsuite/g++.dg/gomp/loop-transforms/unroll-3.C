// { dg-do compile }
// { dg-additional-options "-std=c++11" }
// { dg-additional-options "-fdump-tree-omp_transform_loops -fopt-info-omp-optimized-missed" }
// { dg-additional-options "-fdump-tree-original" }
#include <vector>

extern void dummy (int);

constexpr unsigned fib (unsigned n)
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
// ! { dg-final { scan-tree-dump {#pragma omp loop_transform unroll_partial\(55\)} "original" } }
// ! { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } }
// ! { dg-final { scan-tree-dump-times "dummy" 55 "omp_transform_loops" } }

// There should be one loop that fills the vector ...
// ! { dg-final { scan-tree-dump-times {if \(i.*? <= .+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } }

// ... and one resulting from the lowering of the unrolled loop
// ! { dg-final { scan-tree-dump-times {if \(D\.[0-9]+ < retval.+?.+goto.+else goto.*?$} 1 "omp_transform_loops" } }
