/* Check that sign/zero extensions are emitted where needed when the
   tst Rm,Rn instruction is used.  */
/* { dg-do compile }  */
/* { dg-options "-O1" }  */
/* { dg-final { scan-assembler-times "tst\tr" 8 } }  */
/* { dg-final { scan-assembler-times "mov.b" 4 } }  */
/* { dg-final { scan-assembler-times "mov.w" 4 } }  */
/* { dg-final { scan-assembler-times "extu.b" 4 } }  */
/* { dg-final { scan-assembler-times "extu.w" 2 } }  */

int
test_00 (char* x, char* y)
{
  /* 2x mov.b (sign extending)  */
  return *x & *y ? -40 : 60;
}

int
test_01 (short* x, short* y)
{
  /* 2x mov.w (sign extending)  */
  return *x & *y ? -40 : 60;
}

int
test_02 (char x, char y)
{
  /* 1x extu.b  */
  return x & y ? -40 : 60;
}

int
test_03 (short x, short y)
{
  /* 1x extu.w  */
  return x & y ? -40 : 60;
}

int
test_04 (char* x, unsigned char y)
{
  /* 1x mov.b, 1x extu.b  */
  return *x & y ? -40 : 60;
}

int
test_05 (short* x, unsigned char y)
{
  /* 1x mov.w, 1x extu.b  */
  return *x & y ? -40 : 60;
}

int
test_06 (short x, short* y, int z, int w)
{
  /* 1x mov.w, 1x extu.w  */
  return x & y[0] ? z : w;
}

int
test_07 (char x, char* y, int z, int w)
{
  /* 1x mov.b, 1x extu.b  */
  return x & y[0] ? z : w;
}
