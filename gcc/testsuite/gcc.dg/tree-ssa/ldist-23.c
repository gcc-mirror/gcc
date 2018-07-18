/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

extern void abort (void);

int a[128], b[128], c[128], d[128];

void __attribute__((noinline,noclone))
foo (void)
{
  int i;
  for (i = 0; i < 128; ++i)
    {
      a[i] = a[i] + 1;
      b[i] = d[i];
      c[i] = a[i] / d[i];
    }
}
int main()
{
  int i;
  for (i = 0; i < 128; ++i)
    a[i] = i;
  for (i = 0; i < 128; ++i)
    d[i] = 1;
  foo ();
  if (c[0] != 1)
    abort ();
  return 0;
}

/* XFAILed due to the fix for PR79245.  */
/* { dg-final { scan-tree-dump "split to 1 loops and 1 library call" "ldist" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "generated memcpy" "ldist" { xfail *-*-* }  } } */
