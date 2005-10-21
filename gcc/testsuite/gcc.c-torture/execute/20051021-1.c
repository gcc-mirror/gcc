/* Verify that TRUTH_AND_EXPR is not wrongly changed to TRUTH_ANDIF_EXPR.  */

extern void abort (void);

int count = 0;

int foo1(void)
{
  count++;
  return 0;
}

int foo2(void)
{
  count++;
  return 0;
}

int main(void)
{
  if ((foo1() == 1) & (foo2() == 1))
    abort ();

  if (count != 2)
    abort ();

  return 0;
}
