typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  omp_default_mem_alloc = 1,
  omp_low_lat_mem_alloc = 5,
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

typedef struct omp_alloctrait_t
{
  int key;
  int value;
} omp_alloctrait_t;

void
f ()
{
   omp_alloctrait_t trait[1] = {{1,1}};
   omp_allocator_handle_t my_alloc;
   #pragma omp target uses_allocators(traits(trait) : my_alloc)  /* { dg-error "traits array 'trait' must be of 'const omp_alloctrait_t \\\[\\\]' type" } */
     ;
}

void
g ()
{
   const omp_alloctrait_t trait[1] = {{1,1}};
   omp_allocator_handle_t my_alloc;
   #pragma omp target uses_allocators(traits(trait) : my_alloc)
     ;
}

// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 31 }
