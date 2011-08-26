/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

unsigned long long
foo (unsigned long long a, unsigned char *b, unsigned short *c)
{
  return a + *b * *c;
}

/* { dg-final { scan-assembler "umlal" } } */
