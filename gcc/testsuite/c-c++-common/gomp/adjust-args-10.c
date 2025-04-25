/* Diagnose invalid type in variadic arguments.  */

void v0(int *, ...) {}

#pragma omp declare variant(v0) match(construct={dispatch}) \
  adjust_args(need_device_ptr: 1:omp_num_args)
void b0(int *, ...) {}

void f0(int *p0, int *p1, int *p2, int *p3, int *p4)
{
  #pragma omp dispatch
  b0(p0, p1, p2, p3, p4, 42); /* { dg-error "variadic argument 5 specified in an 'append_args' clause with the 'need_device_ptr' modifier must be of pointer type" "" { xfail *-*-* } } */
}
