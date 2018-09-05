/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt-details-blocks-details" } */

void foo (int *x1, int *x2, int *x3, int *x4, int *y)
{
  int i;
  int a1, a2, a3, b1, b2;

  for (i=0; i<128; i++)
    {
      a1 = x1[i];
      a2 = x2[i];
      a3 = x3[i];
      y[i] = 0;
      if (x4[i] == 0)
	{
	  b1 = a1 + 1;
	  if (a2 > 0)
	    b1++;
	  a1++;
	  if (a3 < 0)
	    b1--;
	  y[i] = b1;
	}
    }
}

/* { dg-final { scan-tree-dump-times "Use predicate of bb" 4 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming counts" 1 "ifcvt" } } */
