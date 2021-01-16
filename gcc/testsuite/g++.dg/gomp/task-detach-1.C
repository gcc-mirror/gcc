// { dg-do compile }
// { dg-options "-fopenmp" }

typedef enum omp_event_handle_t
{
  __omp_event_handle_t_max__ = __UINTPTR_MAX__
} omp_event_handle_t;

template <typename T>
void foo ()
{
  T t;
  #pragma omp task detach (t)
    ;
}

template <typename T>
void bar ()
{
  T t;
  #pragma omp task detach (t) // { dg-error "'detach' clause event handle has type 'int' rather than 'omp_event_handle_t'" }
    ;
}

void f()
{
  foo <omp_event_handle_t> ();
  bar <int> (); // { dg-message "required from here" }
}
