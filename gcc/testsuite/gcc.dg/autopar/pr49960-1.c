/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details -fdump-tree-optimized" } */

#include <stdlib.h>
#include <stdio.h>

int main() 
{
  unsigned int x, y, idx, H = 1024, W = 1024;
  
  int * tmps = (int *)malloc(H*W*sizeof(int));
  
  /* This loop gets parallelized even though output dependences exist 
     between writes to 'tmps' that prevent parallelization. 
     For example: tmps[1] = 1, ..., tmps[1] = 17.  */
  
  for(x = 1; x < H; x++) 
    {
      for(y = 1; y < W; y++) 
	{
	  idx = x*W+y;
	  tmps[idx % 4096] = idx;	  
	}
    }
  
  for(x = 1; x < 8; x++)
    printf("tmps[%d]=%d\n", x, tmps[x]);
  
  return 0;
}
/* Check that no loop gets parallelized.  */

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 0 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 0 "optimized" } } */
