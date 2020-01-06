/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" } */
/* Originally from gcc.dg/vect/pr51583-3.c.  */

int a[8], b[8];

void
f3 (void)
{
  a[0] = b[0] / 8;
  a[1] = b[1] / 4;
  a[2] = b[2] / 8;
  a[3] = b[3] / 4;
  a[4] = b[4] / 8;
  a[5] = b[5] / 4;
  a[6] = b[6] / 8;
  a[7] = b[7] / 4;
}

/* { dg-final { scan-assembler-not {\tasrd\t} } } */
