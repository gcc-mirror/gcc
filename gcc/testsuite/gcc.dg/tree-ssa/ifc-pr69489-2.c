/* { dg-do compile } */
/* { dg-options "-S -O2 -ftree-vectorize -fdump-tree-ifcvt-stats-blocks-details" { target *-*-* } } */

double
foo (const char *u, const char *v, long n)
{
  long i, n1 = 0, n2 = 0;

  for (i = 0; i < n; i++)
    {
      n2 += (u[i] && !v[i]);
      n1 += (!u[i] && v[i]);
    }
  return (2.0 * n2 * n1);
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming frequencies" 1 "ifcvt" } } */
