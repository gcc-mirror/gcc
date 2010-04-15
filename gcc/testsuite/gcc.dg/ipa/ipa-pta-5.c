/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta -fdump-ipa-pta-details" } */

int **x;

static int __attribute__((noinline,noclone))
foo (int **q)
{
  int a = 1;
  **q = 0;
  *x = &a;
  return **q;
}

extern void abort (void);
int main()
{
  int b;
  int *p = &b;
  x = &p;
  if (foo (&p) != 1)
    abort ();
  return 0;
}

/* { dg-final { cleanup-ipa-dump "pta" } } */
