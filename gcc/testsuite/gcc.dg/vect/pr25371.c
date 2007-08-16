/* { dg-do compile } */

void slow_close(int n)
{
  int i;
  double *mm;

  for (i=0;i<2*n;i++)
    for (i=0;i<2*n;i++)
      *(mm+i*2*n+i) = 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
