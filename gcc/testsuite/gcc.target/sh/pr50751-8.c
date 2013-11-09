/* Check that on SH2A the 4 byte movu.b and movu.w displacement insns are
   generated.  This has to be checked with -O2 because some of the patterns
   rely on peepholes.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "movu.b" 4 } } */
/* { dg-final { scan-assembler-times "movu.w" 3 } } */

int
test_00 (unsigned char* x)
{
  /* 1x movu.b  */
  return x[0];
}

int
test_01 (unsigned short* x)
{
  /* 1x movu.w  */
  return x[0];
}

int
test_02 (unsigned char* x)
{
  /* 1x movu.b  */
  return x[1];
}

int
test_03 (unsigned char* x)
{
  /* 1x movu.b  */
  return x[32];
}

int
test_04 (unsigned char* x)
{
  /* 1x movu.b  */
  return x[9000];
}

int
test_05 (unsigned short* x)
{
  /* 1x movu.w  */
  return x[9000];
}

int
test_06 (unsigned char* x, int i)
{
  /* No movu.b expected here.  Should use mov.b (r0,r4) + extu.b instead.  */
  return x[i];
}

int
test_07 (unsigned short* x, int i)
{
  /* No movu.w expected here.  Should use mov.w (r0,r4) + extu.w instead.  */
  return x[i];
}

int
test_08 (unsigned char* x, int c)
{
  /* No movu.b expected here.  Should use post-inc addressing instead.  */
  int s = 0;
  int i;
  for (i = 0; i < c; ++i)
    s += x[i];
  return s;
}

void
test_09 (unsigned char* x, unsigned char* y)
{
  /* No movu.b expected here, since the zero-extension is irrelevant.  */
  x[1] = y[1];
  x[2] = y[2];
}

void
test_10 (unsigned char* x, unsigned short* y)
{
  /* No movu.w expected here, since the zero-extension is irrelevant.  */
  x[1] = y[1];
  x[2] = y[2];
}

int
test_11 (unsigned char* x, unsigned short* y)
{
  /* 1x movu.w  */
  int yy = y[1];
  x[1] = yy;
  return yy;
}
