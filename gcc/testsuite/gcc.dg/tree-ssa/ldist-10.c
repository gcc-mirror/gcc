/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  int a[1000], b[1000], c[1000];

  for (i = 1; i < 1000; i ++)
    {
      a[i] = c[i]; /* S1 */
      b[i] = a[i-1]+1; /* S2 */
    }
  /* Dependences:
     S1->S2 (flow, level 1)

     One partition as A is used in both S1 and S2.
  */

  return a[1000-2] + b[1000-1] + c[1000-2];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
