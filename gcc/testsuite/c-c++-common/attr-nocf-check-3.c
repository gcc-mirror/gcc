/* { dg-do compile } */

int  foo (void) __attribute__ ((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored" } */
void (*foo1) (void) __attribute__((nocf_check)); /* { dg-warning "'nocf_check' attribute ignored" } */
void (*foo2) (void);

int
foo (void) /* The function's address is not tracked.  */
{
  /* This call site is not tracked for
     control-flow instrumentation.  */
  (*foo1)();

  foo1 = foo2;
  /* This call site is still not tracked for
     control-flow instrumentation.  */
  (*foo1)();

  /* This call site is tracked for
     control-flow instrumentation.  */
  (*foo2)();

  foo2 = foo1;
  /* This call site is still tracked for
     control-flow instrumentation.  */
  (*foo2)();

  return 0;
}
