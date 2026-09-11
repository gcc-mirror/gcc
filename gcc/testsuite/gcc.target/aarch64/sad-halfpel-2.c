/* { dg-do compile } */
/* { dg-options "-O3" } */

/* The half-pel kernel of sad-halfpel-1.c, checked for how the operands are
   loaded rather than for the arithmetic: the nine-element group p2[0..8] is
   read by two eight-lane runs, 0..7 and 1..8, neither of which needs a load
   permutation, so both must be loaded at their natural width - no
   per-element loads and no vector composition.  */

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

/* { dg-final { scan-assembler {\turhadd\tv[0-9]+\.8b} } } */
/* { dg-final { scan-assembler-not {\tldrb\t} } } */
/* { dg-final { scan-assembler-not {\tzip1\t} } } */
