/* { dg-do run } */
/* { dg-options "-O -fipa-pta -fdump-ipa-pta2-details" } */

static void __attribute__((noinline,noclone))
foo (int *p)
{
  *p = 1;
}

extern void abort (void);

int main()
{
  int i = 0;
  foo (&i);
  if (i != 1)
    abort ();
  return 0;
}

/* Verify we correctly compute the units ESCAPED set as empty but
   still properly account for the store via *p in foo.  */

/* { dg-final { scan-ipa-dump "ESCAPED = { }" "pta2" } } */
