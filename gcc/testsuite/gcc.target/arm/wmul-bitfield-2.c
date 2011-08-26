/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

struct bf
{
  int a : 3;
  unsigned int b : 15;
  int c : 3;
};

long long
foo (long long a, struct bf b, struct bf c)
{
  return a + b.b * c.c;
}

/* { dg-final { scan-assembler "smlalbb" } } */
