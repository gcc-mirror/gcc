/* { dg-do compile } */
/* { dg-options "" } */

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
