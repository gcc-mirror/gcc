/* Check that no unnecessary sign/zero extensions are being emitted.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-times "extu.b" 2 } }  */
/* { dg-final { scan-assembler-not "extu.w" } }  */
/* { dg-final { scan-assembler-not "exts.b" } }  */
/* { dg-final { scan-assembler-not "exts.w" } }  */
/* { dg-final { scan-assembler-not "movu" } }  */
/* { dg-final { scan-assembler-not "tst\t#255" } }  */

int
test_00 (unsigned char* x, char* xx, int y, int z)
{
  /* If x[0] / b is treated as a non-extended QImode subreg the zero
     test will be a QImode subreg test, which is supposed to ignore
     bits[31:8].  However, since the QImode memory load always sign
     extends, it's also OK to test all the bits.  Thus we don't want
     to see a tst #255 here.  */
  int b = x[0];
  xx[0] = b;
  return b ? y : z;
}

int
test_01 (unsigned char a, unsigned char b, int c, int d)
{
  /* 2x extu.b  */
  if (a == b)
    return c;
  return d;
}

int
test_02 (unsigned char* a, unsigned char* b, int c, int d)
{
  /* 2x mov.b  */
  if (*a != 0 && *b == 0)
    return c;
  return d;
}

int
test_03 (unsigned char* a)
{
  /* 1x mov.b  */
  return *a == 0;
}

int
test_04 (unsigned short* a)
{
  /* 1x mov.w  */
  return *a == 0;
}

unsigned char test_05_var;
int
test_05 (int a, int b, int c, int d)
{
  /* Must not see sign/zero extension here.  */
  test_05_var = (a == b) | (b == c);
  if (test_05_var)
    return d;

  return 0;
}
