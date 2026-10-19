/* Test hierarchical discriminators for RTL loop unrolling.
   { dg-do compile }
   { dg-options "-O2 -g -funroll-loops -fno-tree-vectorize" } */

int a[256];

int
test_rtl_unroll (void)
{
  int sum = 0;
  for (int i = 0; i < 32; i++)
    {
      asm volatile ("" : "+r" (sum));
      sum += a[i] * 3;
    }
  return sum;
}

/* RTL unroll via loop-unroll.cc should assign non-zero copyid
   discriminators via copy_bbs.  */
/* { dg-final { scan-assembler "discriminator (\[1-9\]\[0-9\]*|0x\[1-9a-fA-F\]\[0-9a-fA-F\]*)" { target gas } } } */
