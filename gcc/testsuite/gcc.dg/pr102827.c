/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize --param ssa-name-def-chain-limit=0" } */
/* { dg-additional-options "-mavx" { target { x86_64-*-* i?86-*-* } } } */

void
test_double_double_nugt_var (double *dest, double *src, int b, int i)
{
  while (i < 1)
    {
      dest[i] = b ? src[i] : 0.0;
      ++i;
    }
}
