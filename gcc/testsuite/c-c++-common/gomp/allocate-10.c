/* TODO: enable for C++ once implemented. */
/* { dg-do compile { target c } } */
/* { dg-additional-options "-Wall -fdump-tree-gimple" } */

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_default_mem_alloc = 1,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

void
f()
{
  int n;
  int A[n]; /* { dg-warning "'n' is used uninitialized" } */
  /* { dg-warning "unused variable 'A'" "" { target *-*-* } .-1 } */
}

void
h1()
{
  omp_allocator_handle_t my_handle;
  int B1[3]; /* { dg-warning "'my_handle' is used uninitialized" } */
  /* { dg-warning "variable 'B1' set but not used" "" { target *-*-* } .-1 } */
  #pragma omp allocate(B1) allocator(my_handle)
  B1[0] = 5;
  /* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc" 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times "B1.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 12, my_handle\\);" 1 "gimple" } } */
  /* { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(B1.\[0-9\]+, 0B\\);" 1 "gimple" } } */
}

void
h2()
{
  omp_allocator_handle_t my_handle;
  int B2[3];  /* { dg-warning "unused variable 'B2'" } */
  #pragma omp allocate(B2) allocator(my_handle) /* No warning as 'B2' is unused */
}

void
h3()
{
  omp_allocator_handle_t my_handle;
  int B3[3] = {1,2,3};  /* { dg-warning "unused variable 'B3'" } */
  #pragma omp allocate(B3) allocator(my_handle) /* No warning as 'B3' is unused */
}
