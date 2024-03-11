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
int
sub3 (short mask)
{
  return 1 << ((int)mask & 0x1f);
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

/* Test for <optab>si3_extend. */
int
sub7 (int i, int j) {
  return (i << 10) & j;
}

/* Test for <optab>si3_extend. */
unsigned
sub8 (unsigned i, unsigned j) {
  return (i << 10) & j;
}

/* Test for <optab>si3_extend. */
unsigned
sub9 (unsigned i, unsigned j) {
  return (i >> 10) & j;
}

/* { dg-final { scan-assembler-not {\mandi} } } */
/* { dg-final { scan-assembler-not {\msext\.w\M} } } */
