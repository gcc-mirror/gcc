/* { dg-do compile } */
/* { dg-options "-O0 -march=x86-64" } */

typedef short __attribute__((__vector_size__ (8 * sizeof (short)))) V;
V v, w;

void
foo (void)
{
  w = __builtin_shuffle (v != v, 0 < (V) {}, (V) {192} >> 5);
}

/* { dg-final { scan-assembler-not "punpcklwd" } } */
/* { dg-final { scan-assembler-not "pshufd" } } */
/* { dg-final { scan-assembler-times "pxor\[\\t \]%xmm\[0-9\]+, %xmm\[0-9\]+" 1 } } */
