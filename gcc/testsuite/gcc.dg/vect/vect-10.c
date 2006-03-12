/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#define N 16

short a[N];
short d[N];

int foo ()
{
  int i;
  short b[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  short c[N] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};


  /* Not vectorizable yet (strided access pattern).  */
  for (i = 0; i < N/2; i++)
    {
      a[i] = b[2*i+1] * c[2*i+1] - b[2*i] * c[2*i];
      d[i] = b[2*i] * c[2*i+1] + b[2*i+1] * c[2*i];
    } 

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
