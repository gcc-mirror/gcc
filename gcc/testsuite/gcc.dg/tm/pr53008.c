/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

void __attribute__((transaction_safe)) (*fn)(void);

static void __attribute__((transaction_safe))
foo(void)
{
}

void set_fn(void)
{
  fn = foo;
}
