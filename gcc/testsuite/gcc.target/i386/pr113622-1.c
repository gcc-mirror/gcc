/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -w" } */

typedef float __attribute__ ((vector_size (64))) vec;
register vec a asm("zmm5"), b asm("zmm6"), c asm("zmm7");

void
test (void)
{
  for (int i = 0; i < 8; i++)
    c[i] = a[i] < b[i] ? 0.1 : 0.2;
}
