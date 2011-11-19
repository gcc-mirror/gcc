/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void unsafe(void) __attribute__((transaction_unsafe));

void
f(void)
{
  int a;
  __transaction_atomic {
    a = 1;
    __transaction_atomic {
      __transaction_cancel;
    }
  }
  unsafe();
}
