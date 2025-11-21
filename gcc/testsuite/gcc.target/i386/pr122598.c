/* PR target/122598 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mgfni" } */

typedef char V __attribute__ ((vector_size (64)));

V
foo (V v)
{
  v >>= (V) {5};
  v -= ~0;
  v += (V) {} < v;
  return v;
}
