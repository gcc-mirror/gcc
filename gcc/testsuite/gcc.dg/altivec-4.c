/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-maltivec -O0 -Wall" } */

int __attribute__((mode(V4SI))) x, y;

void
b()
{
  __builtin_altivec_vadduwm (x, y);
}
