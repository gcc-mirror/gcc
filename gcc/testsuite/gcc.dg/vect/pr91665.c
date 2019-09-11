/* PR tree-optimization/91665 */
/* { dg-do compile } */
/* { dg-additional-options "-Ofast" } */

short int v;

void
foo (short int x, short int y)
{
  short int *p = &v;

  x = 1;
  while (x != 0)
    x += ++y || (*p = x);
}
