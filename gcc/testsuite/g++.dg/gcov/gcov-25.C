/* { dg-options "--coverage" } */
/* { dg-do run } */

/* [[attribute]] syntax instead of __attribute__((attr)).  */

[[gnu::suppress_coverage]] int
suppressed_function (int a)
{
  int c;
  int b = a + 1;		/* count(#) */
  a *= 2;			/* count(#) */
  int d = a - 1;		/* count(#) */
  c = a+b+d;			/* count(#) */
  return c;			/* count(#) */
}

int
main ()
{
  suppressed_function (1);
}

/* { dg-final { run-gcov gcov-25.C } } */
