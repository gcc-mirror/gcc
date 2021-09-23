/* PR c++/100319 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

typedef enum omp_event_handle_t
{
  __omp_event_handle_t_max__ = __UINTPTR_MAX__
} omp_event_handle_t;

extern void omp_fulfill_event (omp_event_handle_t);

void f (omp_event_handle_t x, omp_event_handle_t y, int z)
{
  #pragma omp task detach (x) firstprivate (y, z)	/* { dg-bogus "the event handle of a 'detach' clause should not be in a data-sharing clause" } */
    ;

  #pragma omp task detach (x) shared (y)		/* { dg-bogus "the event handle of a 'detach' clause should not be in a data-sharing clause" } */
    ;
}
