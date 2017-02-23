
/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-details-blocks -ftree-loop-if-convert-stores" } */
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
/* Sum is wrong here, but not enough for error to be reported.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming frequencies" 0 "ifcvt" } } */
