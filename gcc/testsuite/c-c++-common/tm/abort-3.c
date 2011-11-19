/* { dg-do compile } */

void f(void)
{
  __transaction_atomic {   /* { dg-error "__transaction_atomic. without trans" } */
    __transaction_cancel;  /* { dg-error "_cancel. without trans" } */
  }
}
