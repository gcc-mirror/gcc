/* PR tree-optimization/114293 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

int
foo (int x)
{
  __builtin_memset (&x, 5, -1);
  return __builtin_strlen ((char *) &x);
}
