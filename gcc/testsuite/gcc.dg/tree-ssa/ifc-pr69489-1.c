/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-ifcvt-stats-blocks-details" { target *-*-* } } */

void foo (int a[], int b[])
{
  int i;
  for (i = 0; i < 100; i++)
    {
      if (a[i] == 0)
	a[i] = b[i]*4;
      else
	a[i] = b[i]*3;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming counts" 1 "ifcvt" } } */
