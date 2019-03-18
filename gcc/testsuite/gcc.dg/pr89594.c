/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-if-convert -ftree-loop-vectorize -fno-tree-ch" } */

int h3;

void
in (void)
{
  long int zr;
  int ee = 0;

  for (zr = 0; zr < 1; zr = h3)
    {
      ee = !!h3 ? zr : 0;

      h3 = 0;
      while (h3 < 0)
	h3 = 0;
    }

  h3 = 0;
  while (h3 < 1)
    h3 = !!ee ? (!!h3 + 1) : 0;
}
