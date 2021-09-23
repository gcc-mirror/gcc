/* PR tree-optimization/100239 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

typedef short __attribute__((__vector_size__ (8 * sizeof (short)))) V;
V v, w;

void
foo (void)
{
  w = __builtin_shuffle (v != v, 0 < (V) {}, (V) {192} >> 5);
}
