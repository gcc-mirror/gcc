/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target arm_dsp } */

long long
foo (long long a, char *b, char *c)
{
  return a + *b * *c;
}

/* smlalbb after zero-extending the chars to HImode, or either signed- or
   unsigned-widening multiply after extending them to SImode.  */
/* { dg-final { scan-assembler {(?:smlalbb|[us]mlal)} } } */
