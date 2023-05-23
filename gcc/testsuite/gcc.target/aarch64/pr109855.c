/* { dg-do compile } */
/* { dg-options "-O" } */

typedef char __attribute__((__vector_size__ (4))) U;
typedef short __attribute__((__vector_size__ (8))) V;

U
foo (V v, V w)
{
  return __builtin_convertvector (w * w + w, U);
}

