/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

unsigned int test3 (unsigned int x, unsigned int y, unsigned int z,
		    unsigned int weight,
		    unsigned int w1, unsigned int w2, unsigned int w3)
{
  unsigned int wtmp1 = w1 * weight;
  unsigned int wtmp2 = w2 * weight;
  unsigned int wtmp3 = w3 * weight;
  unsigned int tmp1 = x * wtmp1;
  unsigned int tmp2 = y * wtmp2;
  unsigned int tmp3 = z * wtmp3;
  return tmp1 + tmp2 + tmp3;
}

/* The multiplication with weight should be un-distributed.
   ???  This pattern is not recognized currently.  */

/* { dg-final { scan-tree-dump-times "\\\*" 4 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
