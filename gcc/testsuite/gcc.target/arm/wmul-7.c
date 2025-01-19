/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

unsigned long long
foo (unsigned long long a, unsigned char *b, unsigned short *c)
{
  return a + *b * *c;
}

/* After zero-extending both to SImode, either signed- or unsigned-widening
   multiply will do.  */
/* { dg-final { scan-assembler {[us]mlal} } } */
