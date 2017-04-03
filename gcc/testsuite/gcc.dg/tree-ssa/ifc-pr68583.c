/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt-blocks-details" } */

void foo (long *a)
{
  int i;
  for (i = 0; i < 100; i+=2)
    {
      long *p = &a[i+1];
      if (a[i] == 0)
	{
	  *p = 2;
	  a[i] = 3;
	}
      else
	{
	  *p = 3;
	  a[i] = 4;
	}
    }
}

/* { dg-final { scan-tree-dump "Applying if-conversion" "ifcvt" } } */
/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming frequencies" 1 "ifcvt" } } */

