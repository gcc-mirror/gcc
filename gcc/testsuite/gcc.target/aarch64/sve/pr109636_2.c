/* { dg-additional-options "-O -mcpu=a64fx" } */

typedef unsigned long long __attribute__((__vector_size__ (16))) V;
typedef unsigned long long __attribute__((__vector_size__ (32))) W;

extern void bar (V v);

void foom (V v, W w)
{
  bar (__builtin_shuffle (v, __builtin_shufflevector ((V){}, w, 4, 5) * v));
}

/* { dg-final { scan-assembler {mul\tz[0-9]+.d, p[0-9]+/m, z[0-9]+.d, z[0-9]+.d} } } */
