/* PR c++/118859 */

/* { dg-do compile { target c++11 } } */

/* Diagnose invalid types in a parameter pack that corresponds to an index
   specified in a need_device_ptr/need_device_addr modified adjust_args clause.
   TODO: Needs more cases, ideally matching the preceding (adjust-args-5.C) test cases.  */

template<typename... Ts>
void v0(Ts...) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
				adjust_args(need_device_ptr: 1) /* { dg-note "parameter specified here" } */
template<typename... Ts>
void b0(Ts...) {} /* { dg-error "parameter specified in an 'adjust_args' clause with the 'need_device_ptr' modifier must be of pointer type" } */

void f0(int p0, int p1)
{
  #pragma omp dispatch
  b0(42, p0); /* { dg-message "required from here" } */
  #pragma omp dispatch
  b0(p1, 42); /* { dg-bogus "required from here" } */
}
