/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-ipa-sra" } */

static int *
__attribute__((noinline,used))
  ox (int *i, int *j)
{
  return i;
}

int a;

int *caller (void)
{
  int b = 10;

  return ox (&a, &b);
}
/* { dg-final { scan-ipa-dump-times "Will split parameter" 0 "sra"  } } */
