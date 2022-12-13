/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra"  } */

volatile int vi;

static void __attribute__((noinline))
foo (int c, int &r)
{
  int i;
  if (c)
    i = r;
  else
    i = 0;
  vi = i;
}

void
bar (int c, int j)
{
  foo (c, j);
}

/* { dg-final { scan-ipa-dump "Will split parameter" "sra" } } */
