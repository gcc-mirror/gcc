/* Verify that the fsca input value is not converted to float and then back
   to int.  Notice that we can't count just "lds" insns because mode switches
   use "lds.l".  */
/* { dg-do compile { target { has_fsca } } }  */
/* { dg-options "-O2 -mfsca -funsafe-math-optimizations -fno-ipa-icf" }  */
/* { dg-final { scan-assembler-times "fsca" 7 } } */
/* { dg-final { scan-assembler-times "shad" 1 } } */
/* { dg-final { scan-assembler-times "lds\tr\[0-9\],fpul" 6 } } */
/* { dg-final { scan-assembler-times "fmul" 2 } } */
/* { dg-final { scan-assembler-times "ftrc" 1 } } */

#include <math.h>

static const float pi = 3.14159265359f;

float
test00 (int x)
{
  /* 1x shad, 1x lds, 1x fsca  */
  return sinf ( (x >> 8) * (2*pi) / (1 << 16));
}

float
test01 (int x)
{
  /* 1x lds, 1x fsca  */
  return sinf (x * (2*pi) / 65536);
}

float
test02 (int x)
{
  /* 1x lds, 1x fsca  */
  return sinf (x * (2*pi / 65536));
}

float
test03 (int x)
{
  /* 1x lds, 1x fsca  */
  float scale = 2*pi / 65536;
  return sinf (x * scale);
}

float
test04 (int x)
{
  /* 1x lds, 1x fsca  */
  return cosf (x / 65536.0f * 2*pi);
}

float
test05 (int x)
{
  /* 1x lds, 1x fsca, 1x fmul  */
  float scale = 2*pi / 65536;
  return sinf (x * scale) * cosf (x * scale);
}

float
test_06 (float x)
{
  /* 1x fmul, 1x ftrc, 1x fsca  */
  return sinf (x);
}
