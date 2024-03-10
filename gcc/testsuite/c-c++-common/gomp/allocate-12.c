/* TODO: enable for C++ once implemented. */
/* { dg-do compile { target c } } */

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_default_mem_alloc = 1,
  omp_low_lat_mem_alloc = 5,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

int
f ()
{
  omp_allocator_handle_t my_allocator;
  int n = 5;  /* { dg-note "to be allocated variable declared here" } */
  my_allocator = omp_default_mem_alloc; /* { dg-note "modified here" } */
  #pragma omp allocate(n) allocator(my_allocator)  /* { dg-error "variable 'my_allocator' used in the 'allocator' clause must not be modified between declaration of 'n' and its 'allocate' directive" } */
  n = 7;
  return n;
}


int
g ()
{
  int n = 5;  /* { dg-note "to be allocated variable declared here" } */
  omp_allocator_handle_t my_allocator = omp_low_lat_mem_alloc;  /* { dg-note "declared here" } */
  #pragma omp allocate(n) allocator(my_allocator)  /* { dg-error "variable 'my_allocator' used in the 'allocator' clause must be declared before 'n'" } */
  n = 7;
  return n;
}

int
h ()
{
  /* my_allocator uninitialized - but only diagnosed in the ME with -Wuninitialized;
     see gomp/allocate-10.c.  */
  omp_allocator_handle_t my_allocator;
  int n = 5;
  #pragma omp allocate(n) allocator(my_allocator)
  n = 7;
  return n;
}
