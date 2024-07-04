/* { dg-do compile } */
/* { dg-options "-msse2 -w" } */

typedef double __attribute__ ((vector_size (16))) vec;
register vec a asm("xmm5"), b asm("xmm6"), c asm("xmm7");

void
test (void)
{
  for (int i = 0; i < 2; i++)
    c[i] = a[i] < b[i] ? 0.1 : 0.2;
}
