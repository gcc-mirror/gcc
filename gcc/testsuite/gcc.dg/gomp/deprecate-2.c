/* { dg-additional-options "-fdiagnostics-show-caret -Wdeprecated-openmp" } */

typedef enum omp_allocator_handle_t
#if __cplusplus >= 201103L
: __UINTPTR_TYPE__
#endif
{
  __omp_allocator_handle_t_max__ = __UINTPTR_MAX__
} omp_allocator_handle_t;

typedef struct omp_alloctrait_t
{
  int key;
  int value;
} omp_alloctrait_t;


void f()
{
  omp_allocator_handle_t myalloc;
  const omp_alloctrait_t mytraits[] = {};
  #pragma omp target uses_allocators(myalloc(mytraits))
/* { dg-begin-multiline-output "" }
   #pragma omp target uses_allocators(myalloc(mytraits))
                                      ^~~~~~~~~~~~~~~~~
                                      -------
                                      traits           : myalloc
   { dg-end-multiline-output "" } */
   ;

// { dg-warning "38: the specification of arguments to 'uses_allocators' where each item is of the form 'allocator\\(traits\\)' is deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* }  22 }


  #pragma omp target uses_allocators  (  myalloc  (  mytraits  )  )
/* { dg-begin-multiline-output "" }
   #pragma omp target uses_allocators  (  myalloc  (  mytraits  )  )
                                          ^~~~~~~~~~~~~~~~~~~~~~~
                                          -------
                                          traits                 : myalloc
   { dg-end-multiline-output "" } */
   ;

// { dg-warning "42: the specification of arguments to 'uses_allocators' where each item is of the form 'allocator\\(traits\\)' is deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" "" { target *-*-* }  34 }
}

// { dg-excess-errors "sorry, unimplemented: 'uses_allocators' clause" }
// { dg-excess-errors "sorry, unimplemented: 'uses_allocators' clause" }
