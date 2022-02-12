/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* Test for <optab>si3_mask_1.  */
extern int k;
void
sub2 (int i, long j)
{
  k = i << (j & 0x1f);
}

/* Test for <optab>si3_extend_mask.  */
unsigned long
sub3 (int mask)
{
  return 1 << (mask & 0xff);
}

/* Test for <optab>si3_extend_mask_1.  */
int
sub4 (int i, int j)
{
  return i << (j & 0x1f);
}

/* Test for <optab>di3_mask.  */
long
sub5 (long i, int j)
{
  char k = j & 0x3f;
  return i << k;
}

/* Test for <optab>di3_mask_1.  */
long
sub6 (long i, long j)
{
  return i << (j & 0x3f);
}
/* { dg-final { scan-assembler-not "andi" } } */
/* { dg-final { scan-assembler-not "sext.w" } } */
