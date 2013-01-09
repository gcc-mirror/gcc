/* PR tree-optimization/55831 */
/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow -ftree-vectorize -Wno-unused-label" } */

int g;
short p, q;

void
foo (void)
{
  short a = p, b = q, i;

  if (a)
    {
    label:
      for (i = 0; i < 8; i++)
	b ^= a++;

      if (!b)
	g = 0;
    }
}

void
bar (void)
{
  short a = p, b = q, i;

  if (a)
    {
    label:
      for (i = 0; i < 8; i++)
	b ^= (a = a + 1);

      if (!b)
	g = 0;
    }
}

