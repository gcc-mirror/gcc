/* { dg-do compile } */
/* Make sure that we don't just crash without -fgnu-tm enabled.  */
/* { dg-options "" } */

int x;

int foo(void)
{
  return __transaction_atomic (x + 1);		/* { dg-error "" } */
}

int bar(void)
{
  return __transaction_relaxed (x + 1);		/* { dg-error "" } */
}
