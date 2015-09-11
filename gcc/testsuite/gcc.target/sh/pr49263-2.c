/* Verify that TST #imm, R0 instruction is generated when QImode or HImode
   values are tested against a negative constant.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "and" } }  */
/* { dg-final { scan-assembler-not "exts" } }  */
/* { dg-final { scan-assembler-times "tst\t#127,r0" 2 } }  */
/* { dg-final { scan-assembler-times "tst\t#255,r0" 1 } }  */
/* { dg-final { scan-assembler-times "65407" 1 } }  */
/* { dg-final { scan-assembler-times "-129" 2 } }  */
/* { dg-final { scan-assembler-times "extu" 1 } }  */

int
test_00 (unsigned char x)
{
  /* 1x tst #127  */
  return x & -129 ? -20 : -40;
}

int
test_01 (signed char x)
{
  /* 1x tst #255  */
  return x & -129 ? -20 : -40;
}

int
test_02 (unsigned short x)
{
  /* 1x tst 65407  */
  return x & -129 ? -20 : -40;
}

int
test_03 (unsigned short* x)
{
  /* 1x tst -129  */
  return x[0] & -129 ? -20 : -40;
}

int
test_04 (unsigned short x)
{
  /* 1x extu.w, 1x tst -129  */
  return x & -129 ? x : -1;
}

int
test_05 (unsigned char* x)
{
  /* 1x tst #127  */
  return x[0] & -129 ? -20 : -40;
}
