/* PR target/101007 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse2" } */

typedef unsigned __attribute__((__vector_size__ (8))) U;
typedef unsigned __attribute__((__vector_size__ (16))) V;
V v;
U *p;

void
foo (void)
{
  *p = (U) __builtin_shufflevector ((V)(0 == (V){} >= 0), v, 4, 2);
}
