/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

typedef enum omp_event_handle_t
{
  __omp_event_handle_t_max__ = __UINTPTR_MAX__
} omp_event_handle_t;

extern void omp_fulfill_event (omp_event_handle_t);

void f (omp_event_handle_t x)
{
  void g (void)
  {
    #pragma omp task detach (x)
      omp_fulfill_event (x);
  }

  g ();
}
