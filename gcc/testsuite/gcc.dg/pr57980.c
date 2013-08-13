/* PR tree-optimization/57980 */
/* { dg-do compile } */
/* { dg-options "-O -foptimize-sibling-calls -w" } */

typedef int V __attribute__ ((vector_size (2 * sizeof (int))));
extern V f (void);

V
bar (void)
{
  return -f ();
}

V
foo (void)
{
  V v = { };
  return v - f ();
}
