/* PR tree-optimization/36991 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef float V __attribute__ ((vector_size (16)));
typedef union { V v[4][4]; } U;

void
foo (float x, float y, U *z)
{
  z->v[1][0] = z->v[0][1] = (V) { x, y, 0, 0 };
}
