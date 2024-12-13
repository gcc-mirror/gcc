void f(int *);
#pragma omp declare variant(f) adjust_args(need_device_addr: x)
/* { dg-error "expected 'nothing' or 'need_device_ptr'" "" { target *-*-* } .-1 }  */
/* { dg-note "'need_device_addr' is not valid for C" "" { target *-*-* } .-2 }  */
void g(int *x);
