/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -msse2" } */

void binarize (int npixels, unsigned char *b)
{
  int i;
  for (i = 0; i < npixels; i++)
    b[i] = (b[i] > 225 ? 0xff : 0);
}
