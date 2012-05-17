/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void __attribute__((transaction_safe))
foo(void);

void __attribute__((transaction_safe))
set_fn(void)
{
  void __attribute__((transaction_safe)) (*fn)(void);
  fn = foo;
  fn();
}

/* { dg-final { scan-assembler "_ITM_getTMCloneSafe" } } */
