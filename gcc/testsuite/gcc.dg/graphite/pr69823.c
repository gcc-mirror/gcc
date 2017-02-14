/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

void
foo (int c, int *p, int *a1, int *a2, int *a3)
{
  int i;

  if (c)
    {
      for (i = 0; i < 8; i++)
	a1[i] = 1;

      if (*p)
	*a2 = 0;
    }

  for (i = 0; i < 8; i++)
    a3[i] = 0;
}
