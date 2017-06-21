/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-O3 -march=core-avx2" } */

unsigned *a;
void
fn1 ()
{
  for (int i; i; ++i)
    {
      unsigned g (a[i] << 8 >> 24);
      a[i] = g;
    }
}
