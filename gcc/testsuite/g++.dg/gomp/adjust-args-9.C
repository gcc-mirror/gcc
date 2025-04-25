/* { dg-do compile { target c++11 } } */

/* Literal numeric range in function template with a parameter pack.

   This case may seem to be unremarkable, but it's a non-dependent numeric range
   in a function template in which we don't know the amount of parameters.
   The way it's handled (at the time of writing) causes some pretty low quality
   diagnostics, hence the seperate test case.

   The numeric range is expanded before the function is instantiated, so the
   fact that it was a numeric range is forgotten by the time the size is known
   and a diagnostic can be issued.  */

template<typename... Ts>
void v0(Ts...) {}
template<typename... Ts>
void v1(Ts...) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1:2)
template<typename... Ts>
void b0(Ts...) {}

#pragma omp declare variant(v1) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1:2) /* { dg-error "parameter list item index is out of range" } */
template<typename... Ts>
void b1(Ts...) {}

void f(int *p)
{
  /* Not out of range.  */
  #pragma omp dispatch
  b0(p, p); /* { dg-bogus "required from here" } */
  #pragma omp dispatch
  b1(p, p); /* { dg-bogus "required from here" } */
  /* Out of range.  */
  #pragma omp dispatch
  b1(p); /* { dg-message "required from here" } */
}
