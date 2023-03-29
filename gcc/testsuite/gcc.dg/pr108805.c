/* { dg-do compile { target longlong64 } } */
/* { dg-options "-O" } */
/* { dg-additional-options "-msse2" { target x86_64-*-* i?86-*-* } } */

typedef __INT8_TYPE__ __attribute__((__vector_size__ (4))) U;
typedef __INT32_TYPE__ __attribute__((__vector_size__ (4))) V;
typedef __UINT64_TYPE__ __attribute__((__vector_size__ (8))) W;

int i;
U h;
W g;

U
foo (void)
{
  W w = i != g;
  V v = __builtin_convertvector (i | w >> 2, V);
  U u = (U) v[0] + h;
  return u;
}
