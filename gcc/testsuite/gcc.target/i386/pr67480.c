/* { dg-do compile } */
/* { dg-options "-mavx512bw -O2 -ftree-vectorize" { target i?86-*-* x86_64-*-* } } */

void
foo(const char *in, char *out, unsigned n)
{
  unsigned i;
  for (i = 0; i < n; i++)
    out[i] &= in[i];
}
