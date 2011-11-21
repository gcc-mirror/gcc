/* PR debug/50827 */
/* { dg-do compile } */
/* { dg-options "-g -O2 -funroll-loops" } */

void
foo (int w, int x, int *y, int *z)
{
  float f;
  while (w--)
    {
      f = x;
      if (y)
	*y = (__INTPTR_TYPE__) y + w;
      if (z)
	*z = w;
    }
}
