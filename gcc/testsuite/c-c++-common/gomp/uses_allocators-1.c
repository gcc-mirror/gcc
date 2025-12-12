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

extern void *omp_alloc (__SIZE_TYPE__, omp_allocator_handle_t);

void
f (omp_allocator_handle_t my_alloc)
{
  #pragma omp target
  {
    int a; /* { dg-error "'my_alloc' in 'allocator' clause inside a target region must be specified in an 'uses_allocators' clause on the 'target' directive" "not yet implemented" { xfail *-*-* } } */
    #pragma omp allocate(a) allocator(my_alloc) /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } }  */
    a  = 5;
    void *prt = omp_alloc(32, my_alloc);
    #pragma omp parallel allocate(allocator(my_alloc) : a) firstprivate(a) /* { dg-error "allocator 'my_alloc' in 'allocate' clause inside a target region must be specified in an 'uses_allocators' clause on the 'target' directive" "not yet implemented" { xfail *-*-* } } */
      a = 7;
  }
}

void
g (omp_allocator_handle_t my_alloc)
{
  /* The following defines a default-mem-space allocator with no extra traits. */
  #pragma omp target uses_allocators(my_alloc)
  {
    int a;
    #pragma omp allocate(a) allocator(my_alloc)  /* { dg-message "sorry, unimplemented: '#pragma omp allocate' not yet supported" "" { target c++ } }  */
    a  = 5;
    void *prt = omp_alloc(32, my_alloc);
    #pragma omp parallel allocate(allocator(my_alloc) : a) firstprivate(a)
      a = 7;
  }
}

// { dg-message "sorry, unimplemented: 'uses_allocators' clause" "" { target *-*-* } 37 }
