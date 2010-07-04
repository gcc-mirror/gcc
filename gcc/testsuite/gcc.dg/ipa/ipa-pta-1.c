/* { dg-do run } */
/* { dg-options "-O -fipa-pta -fdump-ipa-pta-details" } */

static int __attribute__((noinline))
foo (int *p, int *q)
{
  *p = 2;
  *q = 1;
  return *p;
}

static int __attribute__((noinline))
bar (int *p, int *q)
{
  *p = -2;
  *q = -1;
  return *p;
}

static int __attribute__((noinline,noclone))
foobar (int foo_p)
{
  int a;
  int (*fn)(int *, int *);
  if (foo_p)
    fn = foo;
  else
    fn = bar;
  return (*fn)(&a, &a);
}

extern void abort (void);

int main()
{
  if (foobar (1) != 1)
    abort ();

  return 0;
}

/* IPA PTA needs to handle indirect calls properly.  Verify that
   both bar and foo get a (and only a) in their arguments points-to sets.  */

/* { dg-final { scan-ipa-dump "fn_1 = { bar foo }" "pta" } } */
/* { dg-final { scan-ipa-dump "bar.arg0 = { a }" "pta" } } */
/* { dg-final { scan-ipa-dump "bar.arg1 = { a }" "pta" } } */
/* { dg-final { scan-ipa-dump "foo.arg0 = { a }" "pta" } } */
/* { dg-final { scan-ipa-dump "foo.arg1 = { a }" "pta" } } */
/* { dg-final { cleanup-ipa-dump "pta" } } */
