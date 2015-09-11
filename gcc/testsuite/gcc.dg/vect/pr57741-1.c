/* PR tree-optimization/57741 */
/* { dg-do compile } */

void
foo (float *p, float *q, float x)
{
  int i;
  float f = 1.0f, g = 2.0f;
  for (i = 0; i < 1024; i++)
    {
      *p++ = f;
      f += x;
    }
  for (i = 0; i < 1024; i++)
    {
      *q++ = g;
      g += 0.5f;
    }
}

