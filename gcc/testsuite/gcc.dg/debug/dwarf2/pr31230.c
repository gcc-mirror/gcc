/* { dg-do compile } */
/* { dg-options "-gdwarf -dA --param ggc-min-expand=0 --param ggc-min-heapsize=0" } */
/* { dg-final { scan-assembler-times "DIE.*DW_TAG_array_type" 1  } } */
/* { dg-final { scan-assembler-times "DIE.*DW_TAG_subrange_type" 1  } } */

void f1 (void)
{
  char buffer1[100];
}

int f2 (void)
{
  return 0;
}

void f3 (void)
{
  char buffer2[100];
}
