/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-all" } */

void foo (int * __restrict__ ia,
	  int * __restrict__ ib,
	  int * __restrict__ oxa,
	  int * __restrict__ oxb,
	  int * __restrict__ oya,
	  int * __restrict__ oyb)
{
  int i;
  long int mya[52];
  long int myb[52];

  for (i=0; i < 52; i++)
    {
      mya[i] = ia[i] * oxa[i] + ib[i] * oxb[i];
      myb[i] = -ia[i] * oxb[i] + ib[i] * oxa[i];
      oya[i] = 0;
      oyb[i] = myb[i] >> 10;
    }

  /* This loop should be distributed, and the result should look like
     this:
     |  memset (oya, 0, 208);
     |  for (i=0; i < 52; i++)
     |    oyb[i] = -ia[i] * oxb[i] + ib[i] * oxa[i] >> 10;
  */
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 1 "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 1 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
