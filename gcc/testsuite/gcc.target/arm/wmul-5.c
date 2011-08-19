/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (long long a, char *b, char *c)
{
  return a + *b * *c;
}

/* { dg-final { scan-assembler "umlal" } } */
