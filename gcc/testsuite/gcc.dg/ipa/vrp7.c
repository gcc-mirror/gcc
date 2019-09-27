/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp-details" } */

volatile int cond;
int abs (int);

static __attribute__((noinline, noclone))
int foo (int i)
{
  if (i < 5)
    __builtin_abort ();
  return 0;
}

static __attribute__((noinline, noclone))
int bar (int j)
{
  foo (~j);
  foo (abs (j));
  foo (j);
  return 0;
}

int main ()
{
  for (unsigned int i = 0; i < 10; ++i)
    bar (i);

  return 0;
}

/* { dg-final { scan-ipa-dump-times "Setting value range of param 0 \\(now 0\\) \\\[-10, 9\\\]" 1 "cp" } } */
