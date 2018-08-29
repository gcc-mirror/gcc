/* PR tree-optimization/81661 */
/* { dg-do compile } */
/* { dg-options "-O3 -ftrapv" } */

int a, b, c;

void
foo (void)
{
  while (a + c > b)
    a--;
}
