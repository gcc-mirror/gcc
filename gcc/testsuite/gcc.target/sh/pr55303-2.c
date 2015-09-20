/* Verify that for SH2A smax/smin -> cbranch conversion is done properly
   if the clips insn is not used and the expected comparison insns are
   generated.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "cmp/pl" 4 } } */

int
test_00 (int a)
{
  /* 1x cmp/pl  */
  return a >= 0 ? a : 0;
}

int
test_01 (int a)
{
  /* 1x cmp/pl  */
  return a <= 0 ? a : 0;
}

int
test_02 (int a)
{
  /* 1x cmp/pl  */
  return a < 1 ? 1 : a;
}

int
test_03 (int a)
{
  /* 1x cmp/pl  */
  return a < 1 ? a : 1;
}
