/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp-details" } */

volatile int cond;
int abs (int);

volatile int g;

int __attribute__((noinline, noclone))
take_address (int *p)
{
  g = *p;
}

static int __attribute__((noinline, noclone))
foo (int i)
{
  if (i < 5)
    __builtin_abort ();
  return 0;
}

static int __attribute__((noinline, noclone))
bar (int j)
{
  foo (~j);
  foo (abs (j));
  foo (j);
  take_address (&j);
  return 0;
}

int
main ()
{
  for (unsigned int i = 0; i < 10; ++i)
    bar (i);

  return 0;
}

/* { dg-final { scan-ipa-dump-times "Setting value range of param 0 \\(now 0\\) .irange. int \\\[-10, 9\\\]" 1 "cp" } } */
