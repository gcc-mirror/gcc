/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta -fdump-ipa-pta-details -fdump-tree-fre-details" } */

static int __attribute__((noinline,noclone))
foo (int *p, int *q)
{
  *p = 1;
  *q = 0;
  return *p;
}

extern void abort (void);

int main()
{
  int a, b;
  if (foo (&a, &b) != 1)
    abort ();
  return 0;
}

/* Verify we can disambiguate *p and *q in foo.  */

/* { dg-final { scan-ipa-dump "foo.arg0 = &a" "pta" } } */
/* { dg-final { scan-ipa-dump "foo.arg1 = &b" "pta" } } */
/* { dg-final { scan-tree-dump "Replaced \\\*p_1\\\(D\\\) with 1" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
/* { dg-final { cleanup-ipa-dump "pta" } } */
