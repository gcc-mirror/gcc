/* PR tree-optimization/57980 */
/* { dg-do compile } */
/* { dg-options "-O -foptimize-sibling-calls" } */

typedef int V __attribute__ ((vector_size (sizeof (int))));
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
