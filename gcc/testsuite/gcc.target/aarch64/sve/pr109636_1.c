/* { dg-additional-options "-O -mtune=a64fx" } */

typedef unsigned long long __attribute__((__vector_size__ (16))) V;
typedef unsigned long long __attribute__((__vector_size__ (32))) W;

extern void bar (V v);

void foo (V v, W w)
{
  bar (__builtin_shuffle (v, __builtin_shufflevector ((V){}, w, 4, 5) / v));
}

/* { dg-final { scan-assembler {udiv\tz[0-9]+.d, p[0-9]+/m, z[0-9]+.d, z[0-9]+.d} } } */
