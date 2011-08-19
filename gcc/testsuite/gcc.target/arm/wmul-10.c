/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */


unsigned long long
foo (unsigned short a, unsigned short *b, unsigned short *c)
{
  return (unsigned)a + (unsigned long long)*b * (unsigned long long)*c;
}

/* { dg-final { scan-assembler "umlal" } } */
