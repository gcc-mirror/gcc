/* Test hierarchical discriminators for loop unswitching
   (new version gets a copyid via copy_bbs in loop_version).
   { dg-do compile }
   { dg-options "-O2 -g -funswitch-loops -fno-tree-vectorize -fdump-tree-unswitch-optimized" } */

void
test_unswitch (int *a, int *b, int *r, int size, int order)
{
  for (int i = 0; i < size; i++)
    {
      int tmp;
      if (order == 1)
	tmp = -8 * a[i];
      else
	tmp = a[i];
      asm volatile ("" : "+r" (tmp));
      r[i] = 3 * tmp + b[i];
    }
}

/* { dg-final { scan-tree-dump "unswitching loop" "unswitch" } } */
/* { dg-final { scan-assembler "discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" { target gas } } } */
