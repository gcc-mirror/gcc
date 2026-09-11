/* { dg-do compile } */
/* { dg-options "-O3" } */

int
pix_abs8_x2 (const unsigned char *p1, const unsigned char *p2,
	     long stride, int h)
{
  int s = 0;
  for (int i = 0; i < h; i++)
    {
      for (int k = 0; k < 8; k++)
	s += __builtin_abs (p1[k] - ((p2[k] + p2[k + 1] + 1) >> 1));
      p1 += stride;
      p2 += stride;
    }
  return s;
}

/* { dg-final { scan-assembler {\turhadd\t} } } */
/* { dg-final { scan-assembler {\tuabd} } } */
