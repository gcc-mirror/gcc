/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-stats-blocks-details" } */
/* { dg-require-visibility "" } */

struct st
{
  int a[1024];
  int b[1024];
};

struct st s = {0};
int foo (int x)
{
  int i;
  struct st *p = &s;

  for (i = 0; i < 1024; i++)
    {
      if (x > i)
	p->a[i] = p->b[i];
    }

  return 0;
}
/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming counts" 1 "ifcvt" } } */

