/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O2" } */

long long int
foo (long long x, int in1, int in2)
{
  short a = (in1 & 0xffff0000) >> 16;
  short b = (in2 & 0xffff0000) >> 16;

  return x + b * a;
}

/* { dg-final { scan-assembler "smlaltt\\t" } } */
