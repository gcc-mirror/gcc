/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp-details" } */

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
  if (j > 8)
    return foo (j + 2);
  else if (j > 2)
    return foo (j + 3);

  return 0;
}

int main ()
{
  for (unsigned int i =0; i < 1000; ++i)
    bar (i);

  return 0;
}

/* { dg-final { scan-ipa-dump "Setting value range of param 0 \\(now 0\\) \\\[6," "cp" } } */
/* { dg-final { scan-ipa-dump "Setting value range of param 0 \\(now 0\\) \\\[0, 999\\\]" "cp" } } */
