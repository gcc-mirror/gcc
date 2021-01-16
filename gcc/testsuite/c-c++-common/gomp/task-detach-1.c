/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

typedef enum omp_event_handle_t
{
  __omp_event_handle_t_max__ = __UINTPTR_MAX__
} omp_event_handle_t;

extern void omp_fulfill_event (omp_event_handle_t);

void f (omp_event_handle_t x, omp_event_handle_t y, int z)
{
  #pragma omp task detach (x) detach (y) /* { dg-error "too many 'detach' clauses on a task construct" } */
    ;

  #pragma omp task mergeable detach (x) /* { dg-error "'detach' clause must not be used together with 'mergeable' clause" } */
    ;

  #pragma omp task detach (x) mergeable /* { dg-error "'detach' clause must not be used together with 'mergeable' clause" } */
    ;

  #pragma omp task detach (z) /* { dg-error "'detach' clause event handle has type 'int' rather than 'omp_event_handle_t'" } */
    ;

  #pragma omp parallel master default (none) /* { dg-message "enclosing 'parallel'" } */
    #pragma omp task detach (x) /* { dg-error "'x' not specified in enclosing 'parallel'" } */
      ;

  #pragma omp task detach (x) default (none) /* This should work.  */
    omp_fulfill_event (x);

  #pragma omp task detach (x) firstprivate (x) /* { dg-error "the event handle of a 'detach' clause should not be in a data-sharing clause" } */
    ;

  #pragma omp task detach (x) shared (x) /* { dg-error "the event handle of a 'detach' clause should not be in a data-sharing clause" } */
    ;
}
