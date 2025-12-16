/* { dg-do compile } */

int use (int *x);

void foo (int n)
{
  int x = 99;
  use (&x);
}

/* { dg-final { scan-assembler-times ".cfi_mte_tagged_frame" 1 } } */
