/* { dg-do compile } */

void
flarm(void)
{
  static void foo();  /* { dg-error "invalid storage class" } */

  foo();
}

static void
foo(void)
{
}
