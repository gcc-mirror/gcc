/* PR tree-optimization/85156 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int a, b;

int
foo (void)
{
  return __builtin_expect (a ? b != 0 : 0, ({ 1; }));
}
