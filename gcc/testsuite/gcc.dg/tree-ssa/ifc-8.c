
/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-details-blocks" } */
/* { dg-require-visibility "" } */

#define LEN 4096
 __attribute__((visibility("hidden"), aligned (32))) float array[LEN] = {};

void test ()
{
  for (int i = 0; i < LEN; i++)
    {
      if (array[i] > (float)0.)
	array[i] = 3;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming counts" 1 "ifcvt" } } */
