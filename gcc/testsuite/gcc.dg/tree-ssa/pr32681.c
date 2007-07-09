/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -ftree-vectorize" } */
/* { dg-options "-O3 -ffast-math -ftree-vectorize -march=nocona" { target { i?86-*-* x86_64-*-* } } } */

int aa_renderpalette (int p)
{
  int y = 42;
  int i;

  for (i = 0; i < 256; i++)
    {
      if (y > 255)
	y = 255;
      if (y < 0)
	y = 0;

      if (p)
	y = (y < p ? 0 : (y > p) * 255 / (255 - 2 * p));
    }
    return y;
}
