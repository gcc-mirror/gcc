/* PR tree-optimization/109392 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi" } */

typedef short __attribute__ ((__vector_size__ (64))) V;
V v, w;
void bar (void) __attribute__((returns_twice));

V
foo (V a, V b)
{
  bar ();
  b &= v < b;
  return (V) { foo (b, w)[3], (V) {}[3] };
}
