/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -mno-sse2" } */

typedef char __attribute__((__vector_size__ (16))) U;
typedef int __attribute__((__vector_size__ (16))) V;

V v;

U
foo0 (U u)
{
  v *= (V) u & 4;
  return u;
}
