// PR middle-end/113436
// { dg-do "compile" }
// { dg-options "-fopenmp -fdump-tree-omplower" }

// #include <omp.h>
typedef __UINTPTR_TYPE__ omp_uintptr_t;

#if __cplusplus >= 201103L
# define __GOMP_UINTPTR_T_ENUM : omp_uintptr_t
#else
# define __GOMP_UINTPTR_T_ENUM
#endif

typedef enum omp_allocator_handle_t __GOMP_UINTPTR_T_ENUM
{
  omp_null_allocator = 0,
  omp_default_mem_alloc = 1,
  omp_large_cap_mem_alloc = 2,
  omp_const_mem_alloc = 3,
  omp_high_bw_mem_alloc = 4,
  omp_low_lat_mem_alloc = 5,
  omp_cgroup_mem_alloc = 6,
  omp_pteam_mem_alloc = 7,
  omp_thread_mem_alloc = 8,
  ompx_gnu_pinned_mem_alloc = 200,
  ompx_gnu_managed_mem_alloc = 201,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

void f(int x)
{
  int a[x], *b[x];
  int (&aRef)[x] = a;
  int *(&bRef)[x] = b;

  #pragma omp target firstprivate (aRef, bRef) \
		     allocate (allocator (omp_low_lat_mem_alloc): aRef) \
		     allocate (allocator (omp_large_cap_mem_alloc): bRef)
  {
    aRef[0] = 0;
    bRef[0] = 0;
  }
}

// Ensure that the size of memory allocated for the VLA is from a variable
// rather than a constant, and that the default alignments are as expected.

// { dg-final { scan-tree-dump "a\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target int32 } } }
// { dg-final { scan-tree-dump "b\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 32\\\);" "omplower" { target ilp32 } } }
// { dg-final { scan-tree-dump "b\\\.\[0-9\]\+ = __builtin_alloca_with_align \\\(D\\\.\[0-9\]\+, 64\\\);" "omplower" { target { lp64 || llp64 } } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(4, D\\\.\[0-9\]\+, 5\\\);" "omplower" { target int32 } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(4, D\\\.\[0-9\]\+, 2\\\);" "omplower" { target ilp32 } } }
// { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(8, D\\\.\[0-9\]\+, 2\\\);" "omplower" { target { lp64 || llp64 } } } }
// { dg-final { scan-tree-dump "aRef = D\\\.\[0-9\]\+;" "omplower" } }
// { dg-final { scan-tree-dump "bRef = D\\\.\[0-9\]\+;" "omplower" } }
