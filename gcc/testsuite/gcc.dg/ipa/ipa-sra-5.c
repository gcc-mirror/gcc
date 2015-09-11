/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra -fdump-tree-eipa_sra-details" } */

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
/* { dg-final { scan-tree-dump-times "base: j, remove_param" 0 "eipa_sra"  } } */
