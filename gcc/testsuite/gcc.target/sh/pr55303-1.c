/* Verify that the SH2A clips and clipu instructions are generated as
   expected.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "clips.b" 2 } } */
/* { dg-final { scan-assembler-times "clips.w" 2 } } */
/* { dg-final { scan-assembler-times "clipu.b" 2 } } */
/* { dg-final { scan-assembler-times "clipu.w" 2 } } */

static inline int
min (int a, int b)
{
  return a < b ? a : b;
}

static inline int
max (int a, int b)
{
  return a < b ? b : a;
}

int
test_00 (int a)
{
  /* 1x clips.b  */
  return max (-128, min (127, a));
}

int
test_01 (int a)
{
  /* 1x clips.b  */
  return min (127, max (-128, a));
}

int
test_02 (int a)
{
  /* 1x clips.w  */
  return max (-32768, min (32767, a));
}

int
test_03 (int a)
{
  /* 1x clips.w  */
  return min (32767, max (-32768, a));
}

unsigned int
test_04 (unsigned int a)
{
  /* 1x clipu.b  */
  return a > 255 ? 255 : a;
}

unsigned int
test_05 (unsigned int a)
{
  /* 1x clipu.b  */
  return a >= 255 ? 255 : a;
}

unsigned int
test_06 (unsigned int a)
{
  /* 1x clipu.w  */
  return a > 65535 ? 65535 : a;
}

unsigned int
test_07 (unsigned int a)
{
  /* 1x clipu.w  */
  return a >= 65535 ? 65535 : a;
}

void
test_08 (unsigned short a, unsigned short b, unsigned int* r)
{
  /* Must not see a clip insn here -- it is not needed.  */
  unsigned short x = a + b;
  if (x > 65535)
    x = 65535;
  *r = x;
}
