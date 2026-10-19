/* Test hierarchical discriminators for loop header copying.
   { dg-do compile }
   { dg-options "-O2 -g -fno-tree-vectorize -fdump-tree-ch2-details" } */

int a[100];

int
test_ch (int n)
{
  int sum = 0;
  int i = 0;
  while (i < n)
    {
      asm volatile ("" : "+r" (sum));
      sum += a[i];
      i++;
    }
  return sum;
}

/* Header copying peels the exit check; the peeled copy should get a
   non-zero hierarchical discriminator.  */
/* { dg-final { scan-tree-dump "Duplicating header of the loop" "ch2" } } */
/* { dg-final { scan-assembler "discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" { target gas } } } */
