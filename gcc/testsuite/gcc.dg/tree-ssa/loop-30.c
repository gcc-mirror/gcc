/* PR 25371 */

/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void
slow_close(int n)
{
  int i;
  double *mm;
  for (i=0;i<2*n;i++)
    for (i=0;i<2*n;i++)
      *(mm+i*2*n+i) = 0;
}
