/* Verify that TST #imm, R0 instruction is generated when the tested reg
   is shifted by a constant amount.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "and|shl|sha|exts" } }  */

/* { dg-final { scan-assembler-times "tst\t#7,r0" 3 } }  */
/* { dg-final { scan-assembler-times "tst\t#12,r0" 1 } }  */
/* { dg-final { scan-assembler-times "tst\t#24,r0" 6 } }  */
/* { dg-final { scan-assembler-times "tst\t#13,r0" 3 } }  */
/* { dg-final { scan-assembler-times "tst\t#242,r0" 3 } }  */
/* { dg-final { scan-assembler-times "tst\t#252,r0" 1 } }  */

/* { dg-final { scan-assembler-times "tst\t#64,r0" 6 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "tst\t#64,r0" 4 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "bld\t#6" 2 { target { sh2a } } } }  */

int
test_00 (unsigned char* x, int y, int z)
{
  /* 1x tst #12  */
  return (x[0] << 4) & 192 ? y : z;
}

int
test_01 (unsigned char* x, int y, int z)
{
  /* 1x tst #24  */
  return (x[0] << 3) & 192 ? y : z;
}

int
test_02 (unsigned char* x, int y, int z)
{
  /* 1x tst #24  */
  return ((x[0] << 3) & 192) != 0;
}

int
test_03 (unsigned char* x, int y, int z)
{
  /* 1x tst #24  */
  return ((x[0] << 3) & 192) == 0;
}

int
test_04 (unsigned char x, int y, int z)
{
  /* 1x tst #24  */
  return (x << 3) & 192 ? y : z;
}

int
test_05 (unsigned char x, int y, int z)
{
  /* 1x tst #24  */
  return ((x << 3) & 192) != 0;
}

int
test_06 (unsigned char x, int y, int z)
{
  /* 1x tst #24  */
  return ((x << 3) & 192) == 0;
}

int
test_07 (unsigned char x, int y, int z)
{
  /* 1x tst #13  */
  return (x << 3) & 111 ? y : z;
}

int
test_08 (unsigned char x, int y, int z)
{
  /* 1x tst #13  */
  return ((x << 3) & 111) != 0;
}

int
test_09 (unsigned char x, int y, int z)
{
  /* 1x tst #13  */
  return ((x << 3) & 111) == 0;
}

int
test_10 (unsigned char x, int y, int z)
{
  /* 1x tst #242  */
  return (x << 3) & -111 ? y : z;
}

int
test_11 (unsigned char x, int y, int z)
{
  /* 1x tst #242  */
  return ((x << 3) & -111) != 0;
}

int
test_12 (unsigned char x, int y, int z)
{
  /* 1x tst #242  */
  return ((x << 3) & -111) == 0;
}

int
test_13 (unsigned char* x, int y, int z)
{
  /* 1x tst #64  */
  return (x[0] >> 2) & 16 ? y : z;
}

int
test_14 (unsigned char* x, int y, int z)
{
  /* 1x tst #64  / 1x bld #6*/
  return ((x[0] >> 2) & 16) != 0;
}
int
test_15 (unsigned char* x, int y, int z)
{
  /* 1x tst #64  */
  return ((x[0] >> 2) & 16) == 0;
}

int
test_16 (unsigned char x, int y, int z)
{
  /* 1x tst #64  */
  return (x >> 2) & 16 ? y : z;
}

int
test_17 (unsigned char x, int y, int z)
{
  /* 1x tst #64  / 1x bld #6*/
  return ((x >> 2) & 16) != 0;
}

int
test_18 (unsigned char x, int y, int z)
{
  /* 1x tst #64  */
  return ((x >> 2) & 16) == 0;
}

int
test_19 (signed char x, int y, int z)
{
  /* 1x tst #7  */
  return (x << 1) & 0x0F ? y : z;
}

int
test_20 (signed char x, int y, int z)
{
  /* 1x tst #7  */
  return ((x << 1) & 0x0F) != 0;
}

int
test_21 (signed char x, int y, int z)
{
  /* 1x tst #7  */
  return ((x << 1) & 0x0F) == 0;
}

int
test_22 (unsigned char* x, int y, int z)
{
  /* 1x tst #252  */
  return (x[0] >> 2) ? y : z;
}
