/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void IMB_double_fast_x (int * __restrict__ destf,
			int * __restrict__ dest, int y,
			int * __restrict__ p1f)
{
  int i;
  for (i = y; i > 0; i--)
    {
      *dest++ = 0;
      destf[0] = p1f[0];
      destf[1] = p1f[1];
      destf[2] = p1f[2];
      destf[3] = p1f[3];
      destf[4] = p1f[8];
      destf[5] = p1f[9];
      destf[6] = p1f[10];
      destf[7] = p1f[11];
      destf += 8;
      p1f += 12;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { vect_hw_misalign && vect_perm } } } } */
