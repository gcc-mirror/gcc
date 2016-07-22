/* Check that DImode comparisons are optimized as expected when compiling
   with -Os.  */
/* { dg-do compile }  */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-times "tst" 2 } }  */
/* { dg-final { scan-assembler-not "cmp" } }  */

int
test_00 (long long* x)
{
  /* 1x tst, no cmp/* insns.  */
  return *x & 0xFFFFFFFF ? -20 : -40;
}

int
test_01 (unsigned long long x)
{
  /* 1x tst, no cmp/* insns.  */
  return x >= 0x100000000LL ? -20 : -40;
}
