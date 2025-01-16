/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (long long a, unsigned char *b, signed char *c)
{
  return a + (long long)*b * (long long)*c;
}

/* After zero-extending B and sign-extending C to [HS]imode, either
   signed-widening multiply will do.  */
/* { dg-final { scan-assembler {smlal(?:bb)?} } } */
