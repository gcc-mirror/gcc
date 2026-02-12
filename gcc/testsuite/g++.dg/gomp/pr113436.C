// PR middle-end/113436
// { dg-do "compile" }
// { dg-options "-std=gnu++20 -fopenmp -fdump-tree-omplower" }

#include <omp.h>

void f()
{
  int a[10];
  auto &aRef = a;

  #pragma omp target firstprivate(aRef) \
		     allocate(align(128), allocator(omp_low_lat_mem_alloc): aRef)
    aRef[0] = 1;
}

// { dg-final { scan-tree-dump "aRef\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(128, 40, 5\\\);" "omplower" { target int32 } } }
// { dg-final { scan-tree-dump "aRef = aRef\\\.\[0-9\]\+;" "omplower" } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = \\\(\\\*D\\\.\[0-9\]+\\\);" "omplower" } }
// { dg-final { scan-tree-dump "\\\(\\\*aRef\\\) = D\\\.\[0-9\]\+;" "omplower" } }
// { dg-final { scan-tree-dump "\\\(\\\*aRef\\\)\\\[0\\\] = 1;" "omplower" } }
// { dg-final { scan-tree-dump "__builtin_GOMP_free \\\(aRef\\\.\[0-9\]\+, 5\\\);" "omplower" } }
