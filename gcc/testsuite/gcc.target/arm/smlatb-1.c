/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O2" } */

int
foo (int x, int in)
{
  short a = in & 0xffff;
  short b = (in & 0xffff0000) >> 16;

  return x + b * a;
}

/* { dg-final { scan-assembler "smlatb\\t" } } */
