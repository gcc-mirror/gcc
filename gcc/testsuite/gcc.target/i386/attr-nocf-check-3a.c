/* { dg-do compile } */
/* { dg-options "-fpermissive -fcf-protection" } */

int  foo (void) __attribute__ ((nocf_check));
void (*foo1) (void) __attribute__((nocf_check));
void (*foo2) (void);

int __attribute__ ((nocf_check))
foo (void) /* The function's address is not tracked.  */
{
  /* This call site is not tracked for
     control-flow instrumentation.  */
  (*foo1)();

  foo1 = foo2; /* { dg-warning "incompatible pointer type" "" { target c } } */
	       /* { dg-error "invalid conversion" "" { target c++ } .-1 } */
  /* This call site is still not tracked for
     control-flow instrumentation.  */
  (*foo1)();

  /* This call site is tracked for
     control-flow instrumentation.  */
  (*foo2)();

  foo2 = foo1; /* { dg-warning "incompatible pointer type" "" { target c } } */
	       /* { dg-error "invalid conversion" "" { target c++ } .-1 } */
  /* This call site is still tracked for
     control-flow instrumentation.  */
  (*foo2)();

  return 0;
}
