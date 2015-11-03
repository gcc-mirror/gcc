// Transaction-unsafe testcase from TM TS.
// { dg-options -fgnu-tm }

volatile int * p = 0;
__attribute ((transaction_safe))
int f() {
  int x = 0;	     // ok: not volatile
  p = &x;	     // ok: the pointer is not volatile
  int i = *p;	     // { dg-error "volatile" "read through volatile glvalue" }
}
