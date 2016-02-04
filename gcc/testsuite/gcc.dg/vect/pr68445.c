/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void IMB_double_fast_x (int *destf, int *dest, int y, int *p1f)
{
  int i;
  for (i = y; i > 0; i--)
    {
      *dest++ = 0;
      destf[0] = destf[4] = p1f[0];
      destf[1] = destf[5] = p1f[1];
      destf[2] = destf[6] = p1f[2];
      destf[3] = destf[7] = p1f[3];
      destf += 8;
      p1f += 4;
    }
}

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
