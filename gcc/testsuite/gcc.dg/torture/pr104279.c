/* PR tree-optimization/104279 */
/* { dg-do compile } */

unsigned a, b;

int
main ()
{
  b = ~(0 || ~0);
  a = ~b / ~a;
  return 0;
}
