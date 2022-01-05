/* PR target/103894 */
/* { dg-do compile } */
/* { dg-options "-msse -mno-sse2" } */

typedef unsigned char __attribute__((__vector_size__ (32))) V;
typedef unsigned char __attribute__((__vector_size__ (2))) W;

V v;

W foo (W w)
{
  return __builtin_shufflevector (v, w, 3, 4);
}
