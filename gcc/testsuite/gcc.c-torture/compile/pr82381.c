/* PR tree-optimization/82381 */
/* { dg-do compile } */

signed char b, h;
unsigned short c, e;
short int d, f, g;

void
foo ()
{
  if (h)
    {
      short a = -(d + c - b);
      f = e - a - -d;
    }
  if (c)
    g = 0;
}
