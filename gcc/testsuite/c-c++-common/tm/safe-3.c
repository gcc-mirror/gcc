/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void f_extern (void);
void f_first (void);
void f_later (void);

extern int x;

void f_first (void) { x++; }

void __attribute__((transaction_safe))
test_safe (void)
{
  f_extern ();		/* { dg-error "unsafe function call" } */
  f_first ();
  f_later ();
}

void __attribute__((transaction_may_cancel_outer))
test_mco (void)
{
  f_extern ();		/* { dg-error "unsafe function call" } */
  f_first ();
  f_later ();
}

void
test_atomic (void)
{
  __transaction_atomic {
    f_extern ();	/* { dg-error "unsafe function call" } */
    f_first ();
    f_later ();
  }
  __transaction_relaxed {
    f_extern ();
    f_first ();
    f_later ();
  }
  __transaction_atomic [[outer]] {
    f_extern ();	/* { dg-error "unsafe function call" } */
    f_first ();
    f_later ();
  }
}

void f_later () { f_first(); test_safe(); }
