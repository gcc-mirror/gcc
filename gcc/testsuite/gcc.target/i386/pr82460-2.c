/* PR target/82460 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx512vbmi -mprefer-vector-width=none --param=vect-epilogues-nomask=0" } */
/* We want to reuse the permutation mask in the loop, so use vpermt2b rather
   than vpermi2b.  */
/* { dg-final { scan-assembler-not {\mvpermi2b\M} } } */
/* { dg-final { scan-assembler {\mvpermt2b\M} } } */

void
foo (unsigned char *__restrict__ x, const unsigned short *__restrict__ y,
     unsigned long z)
{
  unsigned char *w = x + z;
  do
    *x++ = *y++ >> 8;
  while (x < w);
}
