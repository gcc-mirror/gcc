/* PR tree-optimization/56478 */
/* { dg-do compile } */

int a;

void
foo ()
{
  int b;
  for (b = 0;; b++)
    a = 0 < -__LONG_LONG_MAX__ - 1 - b ? : 0;
}
