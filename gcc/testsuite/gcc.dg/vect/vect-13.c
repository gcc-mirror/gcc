/* { dg-do compile { target powerpc*-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -maltivec" { target powerpc*-*-* } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-stats -msse2" { target i?86-*-* x86_64-*-* } } */


#define N 16

int a[N];
int results[N] = {0,1,2,3,0,0,0,0,0,0,0,0,12,13,14,15};

int main ()
{
  int i;
  int b[N] = {0,1,2,3,-4,-5,-6,-7,-8,-9,-10,-11,12,13,14,15};

  /* Not vectorizable yet (condition in loop).  */
  for (i = 0; i < N; i++)
    {
      a[i] = (b[i] >= 0 ? b[i] : 0);
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } */
