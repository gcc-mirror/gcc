/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize -fno-tree-dominator-opts" } */
/* { dg-require-effective-target lp64 } */

/* A ranger based DOM causes many more SSA names to be exported, which
   causes slp1 to vectorize more things.  Disabling DOM to avoid
   disturbing this test.  */

void
f1 (double *p, double *q, unsigned int n)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < n; i += 1)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

void
f2 (double *p, double *q, unsigned int n)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = 0; i < n; i += 3)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

void
f3 (double *p, double *q, unsigned int start, unsigned int n)
{
  p = (double *) __builtin_assume_aligned (p, sizeof (double) * 2);
  q = (double *) __builtin_assume_aligned (q, sizeof (double) * 2);
  for (unsigned int i = start; i < n; i += 2)
    {
      double a = q[i] + p[i];
      double b = q[i + 1] + p[i + 1];
      q[i] = a;
      q[i + 1] = b;
    }
}

/* { dg-final { scan-tree-dump-not "optimized: basic block" "slp1" } } */
