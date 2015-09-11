/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int foo (int * __restrict__ ia,
	 int * __restrict__ ib,
	 int * __restrict__ oxa,
	 int * __restrict__ oxb)
{
  int i;
  int oya[52], oyb[52];

  for (i=0; i < 52; i++)
    {
      oya[i] = (ia[i] * oxa[i]) >> 10;
      oyb[i] = (ib[i] * oxb[i]) >> 10;
    }

  return oya[22] + oyb[21];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 1 "ldist" } } */
