#include "allocate-allocator-handle.h"

void test0 ()
{
  int A1[5], B1[5];
  #pragma omp allocate(A1) align(128) allocator(omp_default_mem_alloc)

  #ifndef __cplusplus
    _Static_assert (_Alignof(A1) == _Alignof(B1), "wrong alignment");
  #elif __cplusplus >= 201103L
     static_assert (alignof(A1) == alignof(B1), "wrong alignment"); 
  #endif
}

void
test1 ()
{
  int x[5];
  #pragma omp parallel allocate(omp_thread_mem_alloc: x) firstprivate(x)
   x[0] = 1;

  #pragma omp target allocate(omp_thread_mem_alloc: x) firstprivate(x) /* uses_allocators(omp_thread_mem_alloc) */
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'target' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   x[0] = 1;

  #pragma omp taskloop allocate(omp_thread_mem_alloc: x) firstprivate(x)
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   for (int i = 0; i < 5; i++)
     x[i] = i;

  #pragma omp parallel master taskloop simd allocate(omp_thread_mem_alloc: x) firstprivate(x)
   /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
   for (int i = 0; i < 5; i++)
     x[i] = i;

  #pragma omp parallel
  #pragma omp masked
  {
    #pragma omp task allocate(omp_thread_mem_alloc: x) firstprivate(x)
      /* { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'task' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 } */
      x[0] = 1;
  }
}
