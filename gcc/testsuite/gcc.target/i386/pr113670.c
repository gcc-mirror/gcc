/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -fno-vect-cost-model" } */

typedef float __attribute__ ((vector_size (16))) vec;
typedef int __attribute__ ((vector_size (16))) ivec;
ivec x;

void
test (void)
{
  register vec a asm("xmm3"), b asm("xmm4");
  register ivec c asm("xmm5");
  for (int i = 0; i < 4; i++)
    c[i] = a[i] < b[i] ? -1 : 1;
  x = c;
}
