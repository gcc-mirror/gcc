/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-additional-options "-march=skylake-avx512" { target { i?86-*-* x86_64-*-* } } } */

void
fn1 (char *b, char *d, int *c, int i)
{
  for (; i; i++, d++)
    if (b[i])
      *d = c[i];
}
