/* { dg-do compile } */

void
flarm(void)
{
  static void foo();  /* { dg-error "invalid storage class" } */

  foo();
}
