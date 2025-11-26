/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-O3 -march=haswell -m32" } */

typedef int v2ll __attribute__ ((__vector_size__ (2 * sizeof (int))));
typedef unsigned int v2ull __attribute__ ((__vector_size__ (2 * sizeof (int))));
typedef __attribute__ ((__vector_size__ (2 * sizeof (short)))) short v2s;

v2ll
f (v2ull e)
{
  v2s c = (v2s) e[0];
  return (v2ll) {(int) c, 0};
}
